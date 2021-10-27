{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTWireless.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTWireless.Lens
  ( -- * Operations

    -- ** CreateWirelessGatewayTaskDefinition
    createWirelessGatewayTaskDefinition_name,
    createWirelessGatewayTaskDefinition_clientRequestToken,
    createWirelessGatewayTaskDefinition_tags,
    createWirelessGatewayTaskDefinition_update,
    createWirelessGatewayTaskDefinition_autoCreateTasks,
    createWirelessGatewayTaskDefinitionResponse_arn,
    createWirelessGatewayTaskDefinitionResponse_id,
    createWirelessGatewayTaskDefinitionResponse_httpStatus,

    -- ** ResetAllResourceLogLevels
    resetAllResourceLogLevelsResponse_httpStatus,

    -- ** ListServiceProfiles
    listServiceProfiles_nextToken,
    listServiceProfiles_maxResults,
    listServiceProfilesResponse_serviceProfileList,
    listServiceProfilesResponse_nextToken,
    listServiceProfilesResponse_httpStatus,

    -- ** DeleteServiceProfile
    deleteServiceProfile_id,
    deleteServiceProfileResponse_httpStatus,

    -- ** ListDeviceProfiles
    listDeviceProfiles_nextToken,
    listDeviceProfiles_maxResults,
    listDeviceProfilesResponse_deviceProfileList,
    listDeviceProfilesResponse_nextToken,
    listDeviceProfilesResponse_httpStatus,

    -- ** AssociateWirelessGatewayWithThing
    associateWirelessGatewayWithThing_id,
    associateWirelessGatewayWithThing_thingArn,
    associateWirelessGatewayWithThingResponse_httpStatus,

    -- ** DeleteDeviceProfile
    deleteDeviceProfile_id,
    deleteDeviceProfileResponse_httpStatus,

    -- ** AssociateWirelessDeviceWithThing
    associateWirelessDeviceWithThing_id,
    associateWirelessDeviceWithThing_thingArn,
    associateWirelessDeviceWithThingResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateServiceProfile
    createServiceProfile_loRaWAN,
    createServiceProfile_name,
    createServiceProfile_clientRequestToken,
    createServiceProfile_tags,
    createServiceProfileResponse_arn,
    createServiceProfileResponse_id,
    createServiceProfileResponse_httpStatus,

    -- ** AssociateAwsAccountWithPartnerAccount
    associateAwsAccountWithPartnerAccount_clientRequestToken,
    associateAwsAccountWithPartnerAccount_tags,
    associateAwsAccountWithPartnerAccount_sidewalk,
    associateAwsAccountWithPartnerAccountResponse_sidewalk,
    associateAwsAccountWithPartnerAccountResponse_arn,
    associateAwsAccountWithPartnerAccountResponse_httpStatus,

    -- ** ResetResourceLogLevel
    resetResourceLogLevel_resourceIdentifier,
    resetResourceLogLevel_resourceType,
    resetResourceLogLevelResponse_httpStatus,

    -- ** DeleteDestination
    deleteDestination_name,
    deleteDestinationResponse_httpStatus,

    -- ** UpdateDestination
    updateDestination_expressionType,
    updateDestination_expression,
    updateDestination_description,
    updateDestination_roleArn,
    updateDestination_name,
    updateDestinationResponse_httpStatus,

    -- ** GetServiceEndpoint
    getServiceEndpoint_serviceType,
    getServiceEndpointResponse_serviceEndpoint,
    getServiceEndpointResponse_serverTrust,
    getServiceEndpointResponse_serviceType,
    getServiceEndpointResponse_httpStatus,

    -- ** GetServiceProfile
    getServiceProfile_id,
    getServiceProfileResponse_arn,
    getServiceProfileResponse_loRaWAN,
    getServiceProfileResponse_name,
    getServiceProfileResponse_id,
    getServiceProfileResponse_httpStatus,

    -- ** GetWirelessDeviceStatistics
    getWirelessDeviceStatistics_wirelessDeviceId,
    getWirelessDeviceStatisticsResponse_sidewalk,
    getWirelessDeviceStatisticsResponse_loRaWAN,
    getWirelessDeviceStatisticsResponse_lastUplinkReceivedAt,
    getWirelessDeviceStatisticsResponse_wirelessDeviceId,
    getWirelessDeviceStatisticsResponse_httpStatus,

    -- ** GetWirelessGatewayStatistics
    getWirelessGatewayStatistics_wirelessGatewayId,
    getWirelessGatewayStatisticsResponse_connectionStatus,
    getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt,
    getWirelessGatewayStatisticsResponse_wirelessGatewayId,
    getWirelessGatewayStatisticsResponse_httpStatus,

    -- ** GetWirelessGatewayCertificate
    getWirelessGatewayCertificate_id,
    getWirelessGatewayCertificateResponse_iotCertificateId,
    getWirelessGatewayCertificateResponse_loRaWANNetworkServerCertificateId,
    getWirelessGatewayCertificateResponse_httpStatus,

    -- ** GetWirelessGatewayTask
    getWirelessGatewayTask_id,
    getWirelessGatewayTaskResponse_status,
    getWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId,
    getWirelessGatewayTaskResponse_taskCreatedAt,
    getWirelessGatewayTaskResponse_lastUplinkReceivedAt,
    getWirelessGatewayTaskResponse_wirelessGatewayId,
    getWirelessGatewayTaskResponse_httpStatus,

    -- ** GetWirelessGatewayFirmwareInformation
    getWirelessGatewayFirmwareInformation_id,
    getWirelessGatewayFirmwareInformationResponse_loRaWAN,
    getWirelessGatewayFirmwareInformationResponse_httpStatus,

    -- ** ListPartnerAccounts
    listPartnerAccounts_nextToken,
    listPartnerAccounts_maxResults,
    listPartnerAccountsResponse_sidewalk,
    listPartnerAccountsResponse_nextToken,
    listPartnerAccountsResponse_httpStatus,

    -- ** TestWirelessDevice
    testWirelessDevice_id,
    testWirelessDeviceResponse_result,
    testWirelessDeviceResponse_httpStatus,

    -- ** GetWirelessDevice
    getWirelessDevice_identifier,
    getWirelessDevice_identifierType,
    getWirelessDeviceResponse_sidewalk,
    getWirelessDeviceResponse_arn,
    getWirelessDeviceResponse_loRaWAN,
    getWirelessDeviceResponse_thingArn,
    getWirelessDeviceResponse_name,
    getWirelessDeviceResponse_id,
    getWirelessDeviceResponse_type,
    getWirelessDeviceResponse_destinationName,
    getWirelessDeviceResponse_thingName,
    getWirelessDeviceResponse_description,
    getWirelessDeviceResponse_httpStatus,

    -- ** GetWirelessGateway
    getWirelessGateway_identifier,
    getWirelessGateway_identifierType,
    getWirelessGatewayResponse_arn,
    getWirelessGatewayResponse_loRaWAN,
    getWirelessGatewayResponse_thingArn,
    getWirelessGatewayResponse_name,
    getWirelessGatewayResponse_id,
    getWirelessGatewayResponse_thingName,
    getWirelessGatewayResponse_description,
    getWirelessGatewayResponse_httpStatus,

    -- ** GetPartnerAccount
    getPartnerAccount_partnerAccountId,
    getPartnerAccount_partnerType,
    getPartnerAccountResponse_sidewalk,
    getPartnerAccountResponse_accountLinked,
    getPartnerAccountResponse_httpStatus,

    -- ** CreateWirelessGatewayTask
    createWirelessGatewayTask_id,
    createWirelessGatewayTask_wirelessGatewayTaskDefinitionId,
    createWirelessGatewayTaskResponse_status,
    createWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId,
    createWirelessGatewayTaskResponse_httpStatus,

    -- ** DeleteWirelessGateway
    deleteWirelessGateway_id,
    deleteWirelessGatewayResponse_httpStatus,

    -- ** UpdateWirelessGateway
    updateWirelessGateway_name,
    updateWirelessGateway_joinEuiFilters,
    updateWirelessGateway_description,
    updateWirelessGateway_netIdFilters,
    updateWirelessGateway_id,
    updateWirelessGatewayResponse_httpStatus,

    -- ** DisassociateWirelessGatewayFromCertificate
    disassociateWirelessGatewayFromCertificate_id,
    disassociateWirelessGatewayFromCertificateResponse_httpStatus,

    -- ** ListWirelessGatewayTaskDefinitions
    listWirelessGatewayTaskDefinitions_taskDefinitionType,
    listWirelessGatewayTaskDefinitions_nextToken,
    listWirelessGatewayTaskDefinitions_maxResults,
    listWirelessGatewayTaskDefinitionsResponse_taskDefinitions,
    listWirelessGatewayTaskDefinitionsResponse_nextToken,
    listWirelessGatewayTaskDefinitionsResponse_httpStatus,

    -- ** PutResourceLogLevel
    putResourceLogLevel_resourceIdentifier,
    putResourceLogLevel_resourceType,
    putResourceLogLevel_logLevel,
    putResourceLogLevelResponse_httpStatus,

    -- ** CreateWirelessGateway
    createWirelessGateway_name,
    createWirelessGateway_clientRequestToken,
    createWirelessGateway_description,
    createWirelessGateway_tags,
    createWirelessGateway_loRaWAN,
    createWirelessGatewayResponse_arn,
    createWirelessGatewayResponse_id,
    createWirelessGatewayResponse_httpStatus,

    -- ** DeleteWirelessGatewayTask
    deleteWirelessGatewayTask_id,
    deleteWirelessGatewayTaskResponse_httpStatus,

    -- ** CreateWirelessDevice
    createWirelessDevice_loRaWAN,
    createWirelessDevice_name,
    createWirelessDevice_clientRequestToken,
    createWirelessDevice_description,
    createWirelessDevice_tags,
    createWirelessDevice_type,
    createWirelessDevice_destinationName,
    createWirelessDeviceResponse_arn,
    createWirelessDeviceResponse_id,
    createWirelessDeviceResponse_httpStatus,

    -- ** SendDataToWirelessDevice
    sendDataToWirelessDevice_wirelessMetadata,
    sendDataToWirelessDevice_id,
    sendDataToWirelessDevice_transmitMode,
    sendDataToWirelessDevice_payloadData,
    sendDataToWirelessDeviceResponse_messageId,
    sendDataToWirelessDeviceResponse_httpStatus,

    -- ** CreateDeviceProfile
    createDeviceProfile_loRaWAN,
    createDeviceProfile_name,
    createDeviceProfile_clientRequestToken,
    createDeviceProfile_tags,
    createDeviceProfileResponse_arn,
    createDeviceProfileResponse_id,
    createDeviceProfileResponse_httpStatus,

    -- ** DisassociateAwsAccountFromPartnerAccount
    disassociateAwsAccountFromPartnerAccount_partnerAccountId,
    disassociateAwsAccountFromPartnerAccount_partnerType,
    disassociateAwsAccountFromPartnerAccountResponse_httpStatus,

    -- ** UpdateLogLevelsByResourceTypes
    updateLogLevelsByResourceTypes_defaultLogLevel,
    updateLogLevelsByResourceTypes_wirelessGatewayLogOptions,
    updateLogLevelsByResourceTypes_wirelessDeviceLogOptions,
    updateLogLevelsByResourceTypesResponse_httpStatus,

    -- ** GetDestination
    getDestination_name,
    getDestinationResponse_expressionType,
    getDestinationResponse_arn,
    getDestinationResponse_name,
    getDestinationResponse_expression,
    getDestinationResponse_description,
    getDestinationResponse_roleArn,
    getDestinationResponse_httpStatus,

    -- ** GetLogLevelsByResourceTypes
    getLogLevelsByResourceTypesResponse_defaultLogLevel,
    getLogLevelsByResourceTypesResponse_wirelessGatewayLogOptions,
    getLogLevelsByResourceTypesResponse_wirelessDeviceLogOptions,
    getLogLevelsByResourceTypesResponse_httpStatus,

    -- ** ListDestinations
    listDestinations_nextToken,
    listDestinations_maxResults,
    listDestinationsResponse_nextToken,
    listDestinationsResponse_destinationList,
    listDestinationsResponse_httpStatus,

    -- ** GetDeviceProfile
    getDeviceProfile_id,
    getDeviceProfileResponse_arn,
    getDeviceProfileResponse_loRaWAN,
    getDeviceProfileResponse_name,
    getDeviceProfileResponse_id,
    getDeviceProfileResponse_httpStatus,

    -- ** DisassociateWirelessDeviceFromThing
    disassociateWirelessDeviceFromThing_id,
    disassociateWirelessDeviceFromThingResponse_httpStatus,

    -- ** DisassociateWirelessGatewayFromThing
    disassociateWirelessGatewayFromThing_id,
    disassociateWirelessGatewayFromThingResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreateDestination
    createDestination_clientRequestToken,
    createDestination_description,
    createDestination_tags,
    createDestination_name,
    createDestination_expressionType,
    createDestination_expression,
    createDestination_roleArn,
    createDestinationResponse_arn,
    createDestinationResponse_name,
    createDestinationResponse_httpStatus,

    -- ** AssociateWirelessGatewayWithCertificate
    associateWirelessGatewayWithCertificate_id,
    associateWirelessGatewayWithCertificate_iotCertificateId,
    associateWirelessGatewayWithCertificateResponse_iotCertificateId,
    associateWirelessGatewayWithCertificateResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdatePartnerAccount
    updatePartnerAccount_sidewalk,
    updatePartnerAccount_partnerAccountId,
    updatePartnerAccount_partnerType,
    updatePartnerAccountResponse_httpStatus,

    -- ** GetWirelessGatewayTaskDefinition
    getWirelessGatewayTaskDefinition_id,
    getWirelessGatewayTaskDefinitionResponse_arn,
    getWirelessGatewayTaskDefinitionResponse_name,
    getWirelessGatewayTaskDefinitionResponse_autoCreateTasks,
    getWirelessGatewayTaskDefinitionResponse_update,
    getWirelessGatewayTaskDefinitionResponse_httpStatus,

    -- ** GetResourceLogLevel
    getResourceLogLevel_resourceIdentifier,
    getResourceLogLevel_resourceType,
    getResourceLogLevelResponse_logLevel,
    getResourceLogLevelResponse_httpStatus,

    -- ** DeleteWirelessDevice
    deleteWirelessDevice_id,
    deleteWirelessDeviceResponse_httpStatus,

    -- ** UpdateWirelessDevice
    updateWirelessDevice_loRaWAN,
    updateWirelessDevice_name,
    updateWirelessDevice_destinationName,
    updateWirelessDevice_description,
    updateWirelessDevice_id,
    updateWirelessDeviceResponse_httpStatus,

    -- ** ListWirelessGateways
    listWirelessGateways_nextToken,
    listWirelessGateways_maxResults,
    listWirelessGatewaysResponse_wirelessGatewayList,
    listWirelessGatewaysResponse_nextToken,
    listWirelessGatewaysResponse_httpStatus,

    -- ** DeleteWirelessGatewayTaskDefinition
    deleteWirelessGatewayTaskDefinition_id,
    deleteWirelessGatewayTaskDefinitionResponse_httpStatus,

    -- ** ListWirelessDevices
    listWirelessDevices_serviceProfileId,
    listWirelessDevices_deviceProfileId,
    listWirelessDevices_nextToken,
    listWirelessDevices_wirelessDeviceType,
    listWirelessDevices_destinationName,
    listWirelessDevices_maxResults,
    listWirelessDevicesResponse_wirelessDeviceList,
    listWirelessDevicesResponse_nextToken,
    listWirelessDevicesResponse_httpStatus,

    -- * Types

    -- ** AbpV1_0_x
    abpV1_0_x_devAddr,
    abpV1_0_x_sessionKeys,

    -- ** AbpV1_1
    abpV1_1_devAddr,
    abpV1_1_sessionKeys,

    -- ** CertificateList
    certificateList_signingAlg,
    certificateList_value,

    -- ** Destinations
    destinations_expressionType,
    destinations_arn,
    destinations_name,
    destinations_expression,
    destinations_description,
    destinations_roleArn,

    -- ** DeviceProfile
    deviceProfile_arn,
    deviceProfile_name,
    deviceProfile_id,

    -- ** LoRaWANDevice
    loRaWANDevice_abpV1_0_x,
    loRaWANDevice_otaaV1_1,
    loRaWANDevice_serviceProfileId,
    loRaWANDevice_deviceProfileId,
    loRaWANDevice_otaaV1_0_x,
    loRaWANDevice_abpV1_1,
    loRaWANDevice_devEui,

    -- ** LoRaWANDeviceMetadata
    loRaWANDeviceMetadata_frequency,
    loRaWANDeviceMetadata_fPort,
    loRaWANDeviceMetadata_dataRate,
    loRaWANDeviceMetadata_gateways,
    loRaWANDeviceMetadata_timestamp,
    loRaWANDeviceMetadata_devEui,

    -- ** LoRaWANDeviceProfile
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

    -- ** LoRaWANGateway
    loRaWANGateway_rfRegion,
    loRaWANGateway_subBands,
    loRaWANGateway_gatewayEui,
    loRaWANGateway_joinEuiFilters,
    loRaWANGateway_netIdFilters,

    -- ** LoRaWANGatewayCurrentVersion
    loRaWANGatewayCurrentVersion_currentVersion,

    -- ** LoRaWANGatewayMetadata
    loRaWANGatewayMetadata_gatewayEui,
    loRaWANGatewayMetadata_snr,
    loRaWANGatewayMetadata_rssi,

    -- ** LoRaWANGatewayVersion
    loRaWANGatewayVersion_packageVersion,
    loRaWANGatewayVersion_model,
    loRaWANGatewayVersion_station,

    -- ** LoRaWANGetServiceProfileInfo
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

    -- ** LoRaWANListDevice
    loRaWANListDevice_devEui,

    -- ** LoRaWANSendDataToDevice
    loRaWANSendDataToDevice_fPort,

    -- ** LoRaWANServiceProfile
    loRaWANServiceProfile_addGwMetadata,

    -- ** LoRaWANUpdateDevice
    loRaWANUpdateDevice_serviceProfileId,
    loRaWANUpdateDevice_deviceProfileId,

    -- ** LoRaWANUpdateGatewayTaskCreate
    loRaWANUpdateGatewayTaskCreate_updateSignature,
    loRaWANUpdateGatewayTaskCreate_sigKeyCrc,
    loRaWANUpdateGatewayTaskCreate_currentVersion,
    loRaWANUpdateGatewayTaskCreate_updateVersion,

    -- ** LoRaWANUpdateGatewayTaskEntry
    loRaWANUpdateGatewayTaskEntry_currentVersion,
    loRaWANUpdateGatewayTaskEntry_updateVersion,

    -- ** OtaaV1_0_x
    otaaV1_0_x_appEui,
    otaaV1_0_x_appKey,

    -- ** OtaaV1_1
    otaaV1_1_joinEui,
    otaaV1_1_nwkKey,
    otaaV1_1_appKey,

    -- ** ServiceProfile
    serviceProfile_arn,
    serviceProfile_name,
    serviceProfile_id,

    -- ** SessionKeysAbpV1_0_x
    sessionKeysAbpV1_0_x_nwkSKey,
    sessionKeysAbpV1_0_x_appSKey,

    -- ** SessionKeysAbpV1_1
    sessionKeysAbpV1_1_fNwkSIntKey,
    sessionKeysAbpV1_1_sNwkSIntKey,
    sessionKeysAbpV1_1_nwkSEncKey,
    sessionKeysAbpV1_1_appSKey,

    -- ** SidewalkAccountInfo
    sidewalkAccountInfo_amazonId,
    sidewalkAccountInfo_appServerPrivateKey,

    -- ** SidewalkAccountInfoWithFingerprint
    sidewalkAccountInfoWithFingerprint_arn,
    sidewalkAccountInfoWithFingerprint_fingerprint,
    sidewalkAccountInfoWithFingerprint_amazonId,

    -- ** SidewalkDevice
    sidewalkDevice_sidewalkManufacturingSn,
    sidewalkDevice_amazonId,
    sidewalkDevice_deviceCertificates,
    sidewalkDevice_sidewalkId,

    -- ** SidewalkDeviceMetadata
    sidewalkDeviceMetadata_event,
    sidewalkDeviceMetadata_deviceState,
    sidewalkDeviceMetadata_batteryLevel,
    sidewalkDeviceMetadata_rssi,

    -- ** SidewalkListDevice
    sidewalkListDevice_sidewalkManufacturingSn,
    sidewalkListDevice_amazonId,
    sidewalkListDevice_deviceCertificates,
    sidewalkListDevice_sidewalkId,

    -- ** SidewalkSendDataToDevice
    sidewalkSendDataToDevice_messageType,
    sidewalkSendDataToDevice_seq,

    -- ** SidewalkUpdateAccount
    sidewalkUpdateAccount_appServerPrivateKey,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UpdateWirelessGatewayTaskCreate
    updateWirelessGatewayTaskCreate_updateDataSource,
    updateWirelessGatewayTaskCreate_updateDataRole,
    updateWirelessGatewayTaskCreate_loRaWAN,

    -- ** UpdateWirelessGatewayTaskEntry
    updateWirelessGatewayTaskEntry_arn,
    updateWirelessGatewayTaskEntry_loRaWAN,
    updateWirelessGatewayTaskEntry_id,

    -- ** WirelessDeviceEventLogOption
    wirelessDeviceEventLogOption_event,
    wirelessDeviceEventLogOption_logLevel,

    -- ** WirelessDeviceLogOption
    wirelessDeviceLogOption_events,
    wirelessDeviceLogOption_type,
    wirelessDeviceLogOption_logLevel,

    -- ** WirelessDeviceStatistics
    wirelessDeviceStatistics_sidewalk,
    wirelessDeviceStatistics_arn,
    wirelessDeviceStatistics_loRaWAN,
    wirelessDeviceStatistics_name,
    wirelessDeviceStatistics_id,
    wirelessDeviceStatistics_lastUplinkReceivedAt,
    wirelessDeviceStatistics_type,
    wirelessDeviceStatistics_destinationName,

    -- ** WirelessGatewayEventLogOption
    wirelessGatewayEventLogOption_event,
    wirelessGatewayEventLogOption_logLevel,

    -- ** WirelessGatewayLogOption
    wirelessGatewayLogOption_events,
    wirelessGatewayLogOption_type,
    wirelessGatewayLogOption_logLevel,

    -- ** WirelessGatewayStatistics
    wirelessGatewayStatistics_arn,
    wirelessGatewayStatistics_loRaWAN,
    wirelessGatewayStatistics_name,
    wirelessGatewayStatistics_id,
    wirelessGatewayStatistics_lastUplinkReceivedAt,
    wirelessGatewayStatistics_description,

    -- ** WirelessMetadata
    wirelessMetadata_sidewalk,
    wirelessMetadata_loRaWAN,
  )
where

import Network.AWS.IoTWireless.AssociateAwsAccountWithPartnerAccount
import Network.AWS.IoTWireless.AssociateWirelessDeviceWithThing
import Network.AWS.IoTWireless.AssociateWirelessGatewayWithCertificate
import Network.AWS.IoTWireless.AssociateWirelessGatewayWithThing
import Network.AWS.IoTWireless.CreateDestination
import Network.AWS.IoTWireless.CreateDeviceProfile
import Network.AWS.IoTWireless.CreateServiceProfile
import Network.AWS.IoTWireless.CreateWirelessDevice
import Network.AWS.IoTWireless.CreateWirelessGateway
import Network.AWS.IoTWireless.CreateWirelessGatewayTask
import Network.AWS.IoTWireless.CreateWirelessGatewayTaskDefinition
import Network.AWS.IoTWireless.DeleteDestination
import Network.AWS.IoTWireless.DeleteDeviceProfile
import Network.AWS.IoTWireless.DeleteServiceProfile
import Network.AWS.IoTWireless.DeleteWirelessDevice
import Network.AWS.IoTWireless.DeleteWirelessGateway
import Network.AWS.IoTWireless.DeleteWirelessGatewayTask
import Network.AWS.IoTWireless.DeleteWirelessGatewayTaskDefinition
import Network.AWS.IoTWireless.DisassociateAwsAccountFromPartnerAccount
import Network.AWS.IoTWireless.DisassociateWirelessDeviceFromThing
import Network.AWS.IoTWireless.DisassociateWirelessGatewayFromCertificate
import Network.AWS.IoTWireless.DisassociateWirelessGatewayFromThing
import Network.AWS.IoTWireless.GetDestination
import Network.AWS.IoTWireless.GetDeviceProfile
import Network.AWS.IoTWireless.GetLogLevelsByResourceTypes
import Network.AWS.IoTWireless.GetPartnerAccount
import Network.AWS.IoTWireless.GetResourceLogLevel
import Network.AWS.IoTWireless.GetServiceEndpoint
import Network.AWS.IoTWireless.GetServiceProfile
import Network.AWS.IoTWireless.GetWirelessDevice
import Network.AWS.IoTWireless.GetWirelessDeviceStatistics
import Network.AWS.IoTWireless.GetWirelessGateway
import Network.AWS.IoTWireless.GetWirelessGatewayCertificate
import Network.AWS.IoTWireless.GetWirelessGatewayFirmwareInformation
import Network.AWS.IoTWireless.GetWirelessGatewayStatistics
import Network.AWS.IoTWireless.GetWirelessGatewayTask
import Network.AWS.IoTWireless.GetWirelessGatewayTaskDefinition
import Network.AWS.IoTWireless.ListDestinations
import Network.AWS.IoTWireless.ListDeviceProfiles
import Network.AWS.IoTWireless.ListPartnerAccounts
import Network.AWS.IoTWireless.ListServiceProfiles
import Network.AWS.IoTWireless.ListTagsForResource
import Network.AWS.IoTWireless.ListWirelessDevices
import Network.AWS.IoTWireless.ListWirelessGatewayTaskDefinitions
import Network.AWS.IoTWireless.ListWirelessGateways
import Network.AWS.IoTWireless.PutResourceLogLevel
import Network.AWS.IoTWireless.ResetAllResourceLogLevels
import Network.AWS.IoTWireless.ResetResourceLogLevel
import Network.AWS.IoTWireless.SendDataToWirelessDevice
import Network.AWS.IoTWireless.TagResource
import Network.AWS.IoTWireless.TestWirelessDevice
import Network.AWS.IoTWireless.Types.AbpV1_0_x
import Network.AWS.IoTWireless.Types.AbpV1_1
import Network.AWS.IoTWireless.Types.CertificateList
import Network.AWS.IoTWireless.Types.Destinations
import Network.AWS.IoTWireless.Types.DeviceProfile
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
import Network.AWS.IoTWireless.Types.OtaaV1_0_x
import Network.AWS.IoTWireless.Types.OtaaV1_1
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
import Network.AWS.IoTWireless.Types.Tag
import Network.AWS.IoTWireless.Types.UpdateWirelessGatewayTaskCreate
import Network.AWS.IoTWireless.Types.UpdateWirelessGatewayTaskEntry
import Network.AWS.IoTWireless.Types.WirelessDeviceEventLogOption
import Network.AWS.IoTWireless.Types.WirelessDeviceLogOption
import Network.AWS.IoTWireless.Types.WirelessDeviceStatistics
import Network.AWS.IoTWireless.Types.WirelessGatewayEventLogOption
import Network.AWS.IoTWireless.Types.WirelessGatewayLogOption
import Network.AWS.IoTWireless.Types.WirelessGatewayStatistics
import Network.AWS.IoTWireless.Types.WirelessMetadata
import Network.AWS.IoTWireless.UntagResource
import Network.AWS.IoTWireless.UpdateDestination
import Network.AWS.IoTWireless.UpdateLogLevelsByResourceTypes
import Network.AWS.IoTWireless.UpdatePartnerAccount
import Network.AWS.IoTWireless.UpdateWirelessDevice
import Network.AWS.IoTWireless.UpdateWirelessGateway
