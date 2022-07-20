{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTWireless.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Lens
  ( -- * Operations

    -- ** AssociateAwsAccountWithPartnerAccount
    associateAwsAccountWithPartnerAccount_tags,
    associateAwsAccountWithPartnerAccount_clientRequestToken,
    associateAwsAccountWithPartnerAccount_sidewalk,
    associateAwsAccountWithPartnerAccountResponse_arn,
    associateAwsAccountWithPartnerAccountResponse_sidewalk,
    associateAwsAccountWithPartnerAccountResponse_httpStatus,

    -- ** AssociateWirelessDeviceWithThing
    associateWirelessDeviceWithThing_id,
    associateWirelessDeviceWithThing_thingArn,
    associateWirelessDeviceWithThingResponse_httpStatus,

    -- ** AssociateWirelessGatewayWithCertificate
    associateWirelessGatewayWithCertificate_id,
    associateWirelessGatewayWithCertificate_iotCertificateId,
    associateWirelessGatewayWithCertificateResponse_iotCertificateId,
    associateWirelessGatewayWithCertificateResponse_httpStatus,

    -- ** AssociateWirelessGatewayWithThing
    associateWirelessGatewayWithThing_id,
    associateWirelessGatewayWithThing_thingArn,
    associateWirelessGatewayWithThingResponse_httpStatus,

    -- ** CreateDestination
    createDestination_tags,
    createDestination_clientRequestToken,
    createDestination_description,
    createDestination_name,
    createDestination_expressionType,
    createDestination_expression,
    createDestination_roleArn,
    createDestinationResponse_name,
    createDestinationResponse_arn,
    createDestinationResponse_httpStatus,

    -- ** CreateDeviceProfile
    createDeviceProfile_tags,
    createDeviceProfile_name,
    createDeviceProfile_clientRequestToken,
    createDeviceProfile_loRaWAN,
    createDeviceProfileResponse_arn,
    createDeviceProfileResponse_id,
    createDeviceProfileResponse_httpStatus,

    -- ** CreateServiceProfile
    createServiceProfile_tags,
    createServiceProfile_name,
    createServiceProfile_clientRequestToken,
    createServiceProfile_loRaWAN,
    createServiceProfileResponse_arn,
    createServiceProfileResponse_id,
    createServiceProfileResponse_httpStatus,

    -- ** CreateWirelessDevice
    createWirelessDevice_tags,
    createWirelessDevice_name,
    createWirelessDevice_clientRequestToken,
    createWirelessDevice_loRaWAN,
    createWirelessDevice_description,
    createWirelessDevice_type,
    createWirelessDevice_destinationName,
    createWirelessDeviceResponse_arn,
    createWirelessDeviceResponse_id,
    createWirelessDeviceResponse_httpStatus,

    -- ** CreateWirelessGateway
    createWirelessGateway_tags,
    createWirelessGateway_name,
    createWirelessGateway_clientRequestToken,
    createWirelessGateway_description,
    createWirelessGateway_loRaWAN,
    createWirelessGatewayResponse_arn,
    createWirelessGatewayResponse_id,
    createWirelessGatewayResponse_httpStatus,

    -- ** CreateWirelessGatewayTask
    createWirelessGatewayTask_id,
    createWirelessGatewayTask_wirelessGatewayTaskDefinitionId,
    createWirelessGatewayTaskResponse_status,
    createWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId,
    createWirelessGatewayTaskResponse_httpStatus,

    -- ** CreateWirelessGatewayTaskDefinition
    createWirelessGatewayTaskDefinition_tags,
    createWirelessGatewayTaskDefinition_name,
    createWirelessGatewayTaskDefinition_clientRequestToken,
    createWirelessGatewayTaskDefinition_update,
    createWirelessGatewayTaskDefinition_autoCreateTasks,
    createWirelessGatewayTaskDefinitionResponse_arn,
    createWirelessGatewayTaskDefinitionResponse_id,
    createWirelessGatewayTaskDefinitionResponse_httpStatus,

    -- ** DeleteDestination
    deleteDestination_name,
    deleteDestinationResponse_httpStatus,

    -- ** DeleteDeviceProfile
    deleteDeviceProfile_id,
    deleteDeviceProfileResponse_httpStatus,

    -- ** DeleteServiceProfile
    deleteServiceProfile_id,
    deleteServiceProfileResponse_httpStatus,

    -- ** DeleteWirelessDevice
    deleteWirelessDevice_id,
    deleteWirelessDeviceResponse_httpStatus,

    -- ** DeleteWirelessGateway
    deleteWirelessGateway_id,
    deleteWirelessGatewayResponse_httpStatus,

    -- ** DeleteWirelessGatewayTask
    deleteWirelessGatewayTask_id,
    deleteWirelessGatewayTaskResponse_httpStatus,

    -- ** DeleteWirelessGatewayTaskDefinition
    deleteWirelessGatewayTaskDefinition_id,
    deleteWirelessGatewayTaskDefinitionResponse_httpStatus,

    -- ** DisassociateAwsAccountFromPartnerAccount
    disassociateAwsAccountFromPartnerAccount_partnerAccountId,
    disassociateAwsAccountFromPartnerAccount_partnerType,
    disassociateAwsAccountFromPartnerAccountResponse_httpStatus,

    -- ** DisassociateWirelessDeviceFromThing
    disassociateWirelessDeviceFromThing_id,
    disassociateWirelessDeviceFromThingResponse_httpStatus,

    -- ** DisassociateWirelessGatewayFromCertificate
    disassociateWirelessGatewayFromCertificate_id,
    disassociateWirelessGatewayFromCertificateResponse_httpStatus,

    -- ** DisassociateWirelessGatewayFromThing
    disassociateWirelessGatewayFromThing_id,
    disassociateWirelessGatewayFromThingResponse_httpStatus,

    -- ** GetDestination
    getDestination_name,
    getDestinationResponse_name,
    getDestinationResponse_roleArn,
    getDestinationResponse_arn,
    getDestinationResponse_description,
    getDestinationResponse_expression,
    getDestinationResponse_expressionType,
    getDestinationResponse_httpStatus,

    -- ** GetDeviceProfile
    getDeviceProfile_id,
    getDeviceProfileResponse_name,
    getDeviceProfileResponse_loRaWAN,
    getDeviceProfileResponse_arn,
    getDeviceProfileResponse_id,
    getDeviceProfileResponse_httpStatus,

    -- ** GetLogLevelsByResourceTypes
    getLogLevelsByResourceTypesResponse_wirelessGatewayLogOptions,
    getLogLevelsByResourceTypesResponse_wirelessDeviceLogOptions,
    getLogLevelsByResourceTypesResponse_defaultLogLevel,
    getLogLevelsByResourceTypesResponse_httpStatus,

    -- ** GetPartnerAccount
    getPartnerAccount_partnerAccountId,
    getPartnerAccount_partnerType,
    getPartnerAccountResponse_accountLinked,
    getPartnerAccountResponse_sidewalk,
    getPartnerAccountResponse_httpStatus,

    -- ** GetResourceLogLevel
    getResourceLogLevel_resourceIdentifier,
    getResourceLogLevel_resourceType,
    getResourceLogLevelResponse_logLevel,
    getResourceLogLevelResponse_httpStatus,

    -- ** GetServiceEndpoint
    getServiceEndpoint_serviceType,
    getServiceEndpointResponse_serverTrust,
    getServiceEndpointResponse_serviceType,
    getServiceEndpointResponse_serviceEndpoint,
    getServiceEndpointResponse_httpStatus,

    -- ** GetServiceProfile
    getServiceProfile_id,
    getServiceProfileResponse_name,
    getServiceProfileResponse_loRaWAN,
    getServiceProfileResponse_arn,
    getServiceProfileResponse_id,
    getServiceProfileResponse_httpStatus,

    -- ** GetWirelessDevice
    getWirelessDevice_identifier,
    getWirelessDevice_identifierType,
    getWirelessDeviceResponse_name,
    getWirelessDeviceResponse_thingName,
    getWirelessDeviceResponse_type,
    getWirelessDeviceResponse_thingArn,
    getWirelessDeviceResponse_loRaWAN,
    getWirelessDeviceResponse_destinationName,
    getWirelessDeviceResponse_arn,
    getWirelessDeviceResponse_id,
    getWirelessDeviceResponse_description,
    getWirelessDeviceResponse_sidewalk,
    getWirelessDeviceResponse_httpStatus,

    -- ** GetWirelessDeviceStatistics
    getWirelessDeviceStatistics_wirelessDeviceId,
    getWirelessDeviceStatisticsResponse_lastUplinkReceivedAt,
    getWirelessDeviceStatisticsResponse_loRaWAN,
    getWirelessDeviceStatisticsResponse_wirelessDeviceId,
    getWirelessDeviceStatisticsResponse_sidewalk,
    getWirelessDeviceStatisticsResponse_httpStatus,

    -- ** GetWirelessGateway
    getWirelessGateway_identifier,
    getWirelessGateway_identifierType,
    getWirelessGatewayResponse_name,
    getWirelessGatewayResponse_thingName,
    getWirelessGatewayResponse_thingArn,
    getWirelessGatewayResponse_loRaWAN,
    getWirelessGatewayResponse_arn,
    getWirelessGatewayResponse_id,
    getWirelessGatewayResponse_description,
    getWirelessGatewayResponse_httpStatus,

    -- ** GetWirelessGatewayCertificate
    getWirelessGatewayCertificate_id,
    getWirelessGatewayCertificateResponse_iotCertificateId,
    getWirelessGatewayCertificateResponse_loRaWANNetworkServerCertificateId,
    getWirelessGatewayCertificateResponse_httpStatus,

    -- ** GetWirelessGatewayFirmwareInformation
    getWirelessGatewayFirmwareInformation_id,
    getWirelessGatewayFirmwareInformationResponse_loRaWAN,
    getWirelessGatewayFirmwareInformationResponse_httpStatus,

    -- ** GetWirelessGatewayStatistics
    getWirelessGatewayStatistics_wirelessGatewayId,
    getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt,
    getWirelessGatewayStatisticsResponse_wirelessGatewayId,
    getWirelessGatewayStatisticsResponse_connectionStatus,
    getWirelessGatewayStatisticsResponse_httpStatus,

    -- ** GetWirelessGatewayTask
    getWirelessGatewayTask_id,
    getWirelessGatewayTaskResponse_lastUplinkReceivedAt,
    getWirelessGatewayTaskResponse_wirelessGatewayId,
    getWirelessGatewayTaskResponse_status,
    getWirelessGatewayTaskResponse_taskCreatedAt,
    getWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId,
    getWirelessGatewayTaskResponse_httpStatus,

    -- ** GetWirelessGatewayTaskDefinition
    getWirelessGatewayTaskDefinition_id,
    getWirelessGatewayTaskDefinitionResponse_name,
    getWirelessGatewayTaskDefinitionResponse_arn,
    getWirelessGatewayTaskDefinitionResponse_autoCreateTasks,
    getWirelessGatewayTaskDefinitionResponse_update,
    getWirelessGatewayTaskDefinitionResponse_httpStatus,

    -- ** ListDestinations
    listDestinations_nextToken,
    listDestinations_maxResults,
    listDestinationsResponse_nextToken,
    listDestinationsResponse_destinationList,
    listDestinationsResponse_httpStatus,

    -- ** ListDeviceProfiles
    listDeviceProfiles_nextToken,
    listDeviceProfiles_maxResults,
    listDeviceProfilesResponse_nextToken,
    listDeviceProfilesResponse_deviceProfileList,
    listDeviceProfilesResponse_httpStatus,

    -- ** ListPartnerAccounts
    listPartnerAccounts_nextToken,
    listPartnerAccounts_maxResults,
    listPartnerAccountsResponse_nextToken,
    listPartnerAccountsResponse_sidewalk,
    listPartnerAccountsResponse_httpStatus,

    -- ** ListServiceProfiles
    listServiceProfiles_nextToken,
    listServiceProfiles_maxResults,
    listServiceProfilesResponse_nextToken,
    listServiceProfilesResponse_serviceProfileList,
    listServiceProfilesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWirelessDevices
    listWirelessDevices_nextToken,
    listWirelessDevices_wirelessDeviceType,
    listWirelessDevices_destinationName,
    listWirelessDevices_deviceProfileId,
    listWirelessDevices_maxResults,
    listWirelessDevices_serviceProfileId,
    listWirelessDevicesResponse_nextToken,
    listWirelessDevicesResponse_wirelessDeviceList,
    listWirelessDevicesResponse_httpStatus,

    -- ** ListWirelessGatewayTaskDefinitions
    listWirelessGatewayTaskDefinitions_nextToken,
    listWirelessGatewayTaskDefinitions_maxResults,
    listWirelessGatewayTaskDefinitions_taskDefinitionType,
    listWirelessGatewayTaskDefinitionsResponse_nextToken,
    listWirelessGatewayTaskDefinitionsResponse_taskDefinitions,
    listWirelessGatewayTaskDefinitionsResponse_httpStatus,

    -- ** ListWirelessGateways
    listWirelessGateways_nextToken,
    listWirelessGateways_maxResults,
    listWirelessGatewaysResponse_nextToken,
    listWirelessGatewaysResponse_wirelessGatewayList,
    listWirelessGatewaysResponse_httpStatus,

    -- ** PutResourceLogLevel
    putResourceLogLevel_resourceIdentifier,
    putResourceLogLevel_resourceType,
    putResourceLogLevel_logLevel,
    putResourceLogLevelResponse_httpStatus,

    -- ** ResetAllResourceLogLevels
    resetAllResourceLogLevelsResponse_httpStatus,

    -- ** ResetResourceLogLevel
    resetResourceLogLevel_resourceIdentifier,
    resetResourceLogLevel_resourceType,
    resetResourceLogLevelResponse_httpStatus,

    -- ** SendDataToWirelessDevice
    sendDataToWirelessDevice_wirelessMetadata,
    sendDataToWirelessDevice_id,
    sendDataToWirelessDevice_transmitMode,
    sendDataToWirelessDevice_payloadData,
    sendDataToWirelessDeviceResponse_messageId,
    sendDataToWirelessDeviceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TestWirelessDevice
    testWirelessDevice_id,
    testWirelessDeviceResponse_result,
    testWirelessDeviceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDestination
    updateDestination_roleArn,
    updateDestination_description,
    updateDestination_expression,
    updateDestination_expressionType,
    updateDestination_name,
    updateDestinationResponse_httpStatus,

    -- ** UpdateLogLevelsByResourceTypes
    updateLogLevelsByResourceTypes_wirelessGatewayLogOptions,
    updateLogLevelsByResourceTypes_wirelessDeviceLogOptions,
    updateLogLevelsByResourceTypes_defaultLogLevel,
    updateLogLevelsByResourceTypesResponse_httpStatus,

    -- ** UpdatePartnerAccount
    updatePartnerAccount_sidewalk,
    updatePartnerAccount_partnerAccountId,
    updatePartnerAccount_partnerType,
    updatePartnerAccountResponse_httpStatus,

    -- ** UpdateWirelessDevice
    updateWirelessDevice_name,
    updateWirelessDevice_loRaWAN,
    updateWirelessDevice_destinationName,
    updateWirelessDevice_description,
    updateWirelessDevice_id,
    updateWirelessDeviceResponse_httpStatus,

    -- ** UpdateWirelessGateway
    updateWirelessGateway_name,
    updateWirelessGateway_description,
    updateWirelessGateway_netIdFilters,
    updateWirelessGateway_joinEuiFilters,
    updateWirelessGateway_id,
    updateWirelessGatewayResponse_httpStatus,

    -- * Types

    -- ** AbpV1_0_x
    abpV1_0_x_sessionKeys,
    abpV1_0_x_devAddr,

    -- ** AbpV1_1
    abpV1_1_sessionKeys,
    abpV1_1_devAddr,

    -- ** CertificateList
    certificateList_signingAlg,
    certificateList_value,

    -- ** Destinations
    destinations_name,
    destinations_roleArn,
    destinations_arn,
    destinations_description,
    destinations_expression,
    destinations_expressionType,

    -- ** DeviceProfile
    deviceProfile_name,
    deviceProfile_arn,
    deviceProfile_id,

    -- ** LoRaWANDevice
    loRaWANDevice_otaaV1_1,
    loRaWANDevice_otaaV1_0_x,
    loRaWANDevice_abpV1_0_x,
    loRaWANDevice_deviceProfileId,
    loRaWANDevice_serviceProfileId,
    loRaWANDevice_abpV1_1,
    loRaWANDevice_devEui,

    -- ** LoRaWANDeviceMetadata
    loRaWANDeviceMetadata_fPort,
    loRaWANDeviceMetadata_frequency,
    loRaWANDeviceMetadata_gateways,
    loRaWANDeviceMetadata_timestamp,
    loRaWANDeviceMetadata_dataRate,
    loRaWANDeviceMetadata_devEui,

    -- ** LoRaWANDeviceProfile
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

    -- ** LoRaWANGateway
    loRaWANGateway_rfRegion,
    loRaWANGateway_subBands,
    loRaWANGateway_gatewayEui,
    loRaWANGateway_netIdFilters,
    loRaWANGateway_joinEuiFilters,

    -- ** LoRaWANGatewayCurrentVersion
    loRaWANGatewayCurrentVersion_currentVersion,

    -- ** LoRaWANGatewayMetadata
    loRaWANGatewayMetadata_snr,
    loRaWANGatewayMetadata_gatewayEui,
    loRaWANGatewayMetadata_rssi,

    -- ** LoRaWANGatewayVersion
    loRaWANGatewayVersion_model,
    loRaWANGatewayVersion_station,
    loRaWANGatewayVersion_packageVersion,

    -- ** LoRaWANGetServiceProfileInfo
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

    -- ** LoRaWANListDevice
    loRaWANListDevice_devEui,

    -- ** LoRaWANSendDataToDevice
    loRaWANSendDataToDevice_fPort,

    -- ** LoRaWANServiceProfile
    loRaWANServiceProfile_addGwMetadata,

    -- ** LoRaWANUpdateDevice
    loRaWANUpdateDevice_deviceProfileId,
    loRaWANUpdateDevice_serviceProfileId,

    -- ** LoRaWANUpdateGatewayTaskCreate
    loRaWANUpdateGatewayTaskCreate_sigKeyCrc,
    loRaWANUpdateGatewayTaskCreate_currentVersion,
    loRaWANUpdateGatewayTaskCreate_updateSignature,
    loRaWANUpdateGatewayTaskCreate_updateVersion,

    -- ** LoRaWANUpdateGatewayTaskEntry
    loRaWANUpdateGatewayTaskEntry_currentVersion,
    loRaWANUpdateGatewayTaskEntry_updateVersion,

    -- ** OtaaV1_0_x
    otaaV1_0_x_appKey,
    otaaV1_0_x_appEui,

    -- ** OtaaV1_1
    otaaV1_1_appKey,
    otaaV1_1_nwkKey,
    otaaV1_1_joinEui,

    -- ** ServiceProfile
    serviceProfile_name,
    serviceProfile_arn,
    serviceProfile_id,

    -- ** SessionKeysAbpV1_0_x
    sessionKeysAbpV1_0_x_nwkSKey,
    sessionKeysAbpV1_0_x_appSKey,

    -- ** SessionKeysAbpV1_1
    sessionKeysAbpV1_1_nwkSEncKey,
    sessionKeysAbpV1_1_fNwkSIntKey,
    sessionKeysAbpV1_1_sNwkSIntKey,
    sessionKeysAbpV1_1_appSKey,

    -- ** SidewalkAccountInfo
    sidewalkAccountInfo_appServerPrivateKey,
    sidewalkAccountInfo_amazonId,

    -- ** SidewalkAccountInfoWithFingerprint
    sidewalkAccountInfoWithFingerprint_arn,
    sidewalkAccountInfoWithFingerprint_amazonId,
    sidewalkAccountInfoWithFingerprint_fingerprint,

    -- ** SidewalkDevice
    sidewalkDevice_deviceCertificates,
    sidewalkDevice_sidewalkId,
    sidewalkDevice_amazonId,
    sidewalkDevice_sidewalkManufacturingSn,

    -- ** SidewalkDeviceMetadata
    sidewalkDeviceMetadata_deviceState,
    sidewalkDeviceMetadata_batteryLevel,
    sidewalkDeviceMetadata_event,
    sidewalkDeviceMetadata_rssi,

    -- ** SidewalkListDevice
    sidewalkListDevice_deviceCertificates,
    sidewalkListDevice_sidewalkId,
    sidewalkListDevice_amazonId,
    sidewalkListDevice_sidewalkManufacturingSn,

    -- ** SidewalkSendDataToDevice
    sidewalkSendDataToDevice_seq,
    sidewalkSendDataToDevice_messageType,

    -- ** SidewalkUpdateAccount
    sidewalkUpdateAccount_appServerPrivateKey,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UpdateWirelessGatewayTaskCreate
    updateWirelessGatewayTaskCreate_updateDataRole,
    updateWirelessGatewayTaskCreate_loRaWAN,
    updateWirelessGatewayTaskCreate_updateDataSource,

    -- ** UpdateWirelessGatewayTaskEntry
    updateWirelessGatewayTaskEntry_loRaWAN,
    updateWirelessGatewayTaskEntry_arn,
    updateWirelessGatewayTaskEntry_id,

    -- ** WirelessDeviceEventLogOption
    wirelessDeviceEventLogOption_event,
    wirelessDeviceEventLogOption_logLevel,

    -- ** WirelessDeviceLogOption
    wirelessDeviceLogOption_events,
    wirelessDeviceLogOption_type,
    wirelessDeviceLogOption_logLevel,

    -- ** WirelessDeviceStatistics
    wirelessDeviceStatistics_name,
    wirelessDeviceStatistics_type,
    wirelessDeviceStatistics_lastUplinkReceivedAt,
    wirelessDeviceStatistics_loRaWAN,
    wirelessDeviceStatistics_destinationName,
    wirelessDeviceStatistics_arn,
    wirelessDeviceStatistics_id,
    wirelessDeviceStatistics_sidewalk,

    -- ** WirelessGatewayEventLogOption
    wirelessGatewayEventLogOption_event,
    wirelessGatewayEventLogOption_logLevel,

    -- ** WirelessGatewayLogOption
    wirelessGatewayLogOption_events,
    wirelessGatewayLogOption_type,
    wirelessGatewayLogOption_logLevel,

    -- ** WirelessGatewayStatistics
    wirelessGatewayStatistics_name,
    wirelessGatewayStatistics_lastUplinkReceivedAt,
    wirelessGatewayStatistics_loRaWAN,
    wirelessGatewayStatistics_arn,
    wirelessGatewayStatistics_id,
    wirelessGatewayStatistics_description,

    -- ** WirelessMetadata
    wirelessMetadata_loRaWAN,
    wirelessMetadata_sidewalk,
  )
where

import Amazonka.IoTWireless.AssociateAwsAccountWithPartnerAccount
import Amazonka.IoTWireless.AssociateWirelessDeviceWithThing
import Amazonka.IoTWireless.AssociateWirelessGatewayWithCertificate
import Amazonka.IoTWireless.AssociateWirelessGatewayWithThing
import Amazonka.IoTWireless.CreateDestination
import Amazonka.IoTWireless.CreateDeviceProfile
import Amazonka.IoTWireless.CreateServiceProfile
import Amazonka.IoTWireless.CreateWirelessDevice
import Amazonka.IoTWireless.CreateWirelessGateway
import Amazonka.IoTWireless.CreateWirelessGatewayTask
import Amazonka.IoTWireless.CreateWirelessGatewayTaskDefinition
import Amazonka.IoTWireless.DeleteDestination
import Amazonka.IoTWireless.DeleteDeviceProfile
import Amazonka.IoTWireless.DeleteServiceProfile
import Amazonka.IoTWireless.DeleteWirelessDevice
import Amazonka.IoTWireless.DeleteWirelessGateway
import Amazonka.IoTWireless.DeleteWirelessGatewayTask
import Amazonka.IoTWireless.DeleteWirelessGatewayTaskDefinition
import Amazonka.IoTWireless.DisassociateAwsAccountFromPartnerAccount
import Amazonka.IoTWireless.DisassociateWirelessDeviceFromThing
import Amazonka.IoTWireless.DisassociateWirelessGatewayFromCertificate
import Amazonka.IoTWireless.DisassociateWirelessGatewayFromThing
import Amazonka.IoTWireless.GetDestination
import Amazonka.IoTWireless.GetDeviceProfile
import Amazonka.IoTWireless.GetLogLevelsByResourceTypes
import Amazonka.IoTWireless.GetPartnerAccount
import Amazonka.IoTWireless.GetResourceLogLevel
import Amazonka.IoTWireless.GetServiceEndpoint
import Amazonka.IoTWireless.GetServiceProfile
import Amazonka.IoTWireless.GetWirelessDevice
import Amazonka.IoTWireless.GetWirelessDeviceStatistics
import Amazonka.IoTWireless.GetWirelessGateway
import Amazonka.IoTWireless.GetWirelessGatewayCertificate
import Amazonka.IoTWireless.GetWirelessGatewayFirmwareInformation
import Amazonka.IoTWireless.GetWirelessGatewayStatistics
import Amazonka.IoTWireless.GetWirelessGatewayTask
import Amazonka.IoTWireless.GetWirelessGatewayTaskDefinition
import Amazonka.IoTWireless.ListDestinations
import Amazonka.IoTWireless.ListDeviceProfiles
import Amazonka.IoTWireless.ListPartnerAccounts
import Amazonka.IoTWireless.ListServiceProfiles
import Amazonka.IoTWireless.ListTagsForResource
import Amazonka.IoTWireless.ListWirelessDevices
import Amazonka.IoTWireless.ListWirelessGatewayTaskDefinitions
import Amazonka.IoTWireless.ListWirelessGateways
import Amazonka.IoTWireless.PutResourceLogLevel
import Amazonka.IoTWireless.ResetAllResourceLogLevels
import Amazonka.IoTWireless.ResetResourceLogLevel
import Amazonka.IoTWireless.SendDataToWirelessDevice
import Amazonka.IoTWireless.TagResource
import Amazonka.IoTWireless.TestWirelessDevice
import Amazonka.IoTWireless.Types.AbpV1_0_x
import Amazonka.IoTWireless.Types.AbpV1_1
import Amazonka.IoTWireless.Types.CertificateList
import Amazonka.IoTWireless.Types.Destinations
import Amazonka.IoTWireless.Types.DeviceProfile
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
import Amazonka.IoTWireless.Types.OtaaV1_0_x
import Amazonka.IoTWireless.Types.OtaaV1_1
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
import Amazonka.IoTWireless.Types.Tag
import Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskCreate
import Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskEntry
import Amazonka.IoTWireless.Types.WirelessDeviceEventLogOption
import Amazonka.IoTWireless.Types.WirelessDeviceLogOption
import Amazonka.IoTWireless.Types.WirelessDeviceStatistics
import Amazonka.IoTWireless.Types.WirelessGatewayEventLogOption
import Amazonka.IoTWireless.Types.WirelessGatewayLogOption
import Amazonka.IoTWireless.Types.WirelessGatewayStatistics
import Amazonka.IoTWireless.Types.WirelessMetadata
import Amazonka.IoTWireless.UntagResource
import Amazonka.IoTWireless.UpdateDestination
import Amazonka.IoTWireless.UpdateLogLevelsByResourceTypes
import Amazonka.IoTWireless.UpdatePartnerAccount
import Amazonka.IoTWireless.UpdateWirelessDevice
import Amazonka.IoTWireless.UpdateWirelessGateway
