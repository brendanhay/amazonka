{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTWireless.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Lens
  ( -- * Operations

    -- ** AssociateAwsAccountWithPartnerAccount
    associateAwsAccountWithPartnerAccount_clientRequestToken,
    associateAwsAccountWithPartnerAccount_tags,
    associateAwsAccountWithPartnerAccount_sidewalk,
    associateAwsAccountWithPartnerAccountResponse_arn,
    associateAwsAccountWithPartnerAccountResponse_sidewalk,
    associateAwsAccountWithPartnerAccountResponse_httpStatus,

    -- ** AssociateMulticastGroupWithFuotaTask
    associateMulticastGroupWithFuotaTask_id,
    associateMulticastGroupWithFuotaTask_multicastGroupId,
    associateMulticastGroupWithFuotaTaskResponse_httpStatus,

    -- ** AssociateWirelessDeviceWithFuotaTask
    associateWirelessDeviceWithFuotaTask_id,
    associateWirelessDeviceWithFuotaTask_wirelessDeviceId,
    associateWirelessDeviceWithFuotaTaskResponse_httpStatus,

    -- ** AssociateWirelessDeviceWithMulticastGroup
    associateWirelessDeviceWithMulticastGroup_id,
    associateWirelessDeviceWithMulticastGroup_wirelessDeviceId,
    associateWirelessDeviceWithMulticastGroupResponse_httpStatus,

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

    -- ** CancelMulticastGroupSession
    cancelMulticastGroupSession_id,
    cancelMulticastGroupSessionResponse_httpStatus,

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

    -- ** CreateDeviceProfile
    createDeviceProfile_clientRequestToken,
    createDeviceProfile_loRaWAN,
    createDeviceProfile_name,
    createDeviceProfile_sidewalk,
    createDeviceProfile_tags,
    createDeviceProfileResponse_arn,
    createDeviceProfileResponse_id,
    createDeviceProfileResponse_httpStatus,

    -- ** CreateFuotaTask
    createFuotaTask_clientRequestToken,
    createFuotaTask_description,
    createFuotaTask_fragmentIntervalMS,
    createFuotaTask_fragmentSizeBytes,
    createFuotaTask_loRaWAN,
    createFuotaTask_name,
    createFuotaTask_redundancyPercent,
    createFuotaTask_tags,
    createFuotaTask_firmwareUpdateImage,
    createFuotaTask_firmwareUpdateRole,
    createFuotaTaskResponse_arn,
    createFuotaTaskResponse_id,
    createFuotaTaskResponse_httpStatus,

    -- ** CreateMulticastGroup
    createMulticastGroup_clientRequestToken,
    createMulticastGroup_description,
    createMulticastGroup_name,
    createMulticastGroup_tags,
    createMulticastGroup_loRaWAN,
    createMulticastGroupResponse_arn,
    createMulticastGroupResponse_id,
    createMulticastGroupResponse_httpStatus,

    -- ** CreateNetworkAnalyzerConfiguration
    createNetworkAnalyzerConfiguration_clientRequestToken,
    createNetworkAnalyzerConfiguration_description,
    createNetworkAnalyzerConfiguration_multicastGroups,
    createNetworkAnalyzerConfiguration_tags,
    createNetworkAnalyzerConfiguration_traceContent,
    createNetworkAnalyzerConfiguration_wirelessDevices,
    createNetworkAnalyzerConfiguration_wirelessGateways,
    createNetworkAnalyzerConfiguration_name,
    createNetworkAnalyzerConfigurationResponse_arn,
    createNetworkAnalyzerConfigurationResponse_name,
    createNetworkAnalyzerConfigurationResponse_httpStatus,

    -- ** CreateServiceProfile
    createServiceProfile_clientRequestToken,
    createServiceProfile_loRaWAN,
    createServiceProfile_name,
    createServiceProfile_tags,
    createServiceProfileResponse_arn,
    createServiceProfileResponse_id,
    createServiceProfileResponse_httpStatus,

    -- ** CreateWirelessDevice
    createWirelessDevice_clientRequestToken,
    createWirelessDevice_description,
    createWirelessDevice_loRaWAN,
    createWirelessDevice_name,
    createWirelessDevice_positioning,
    createWirelessDevice_sidewalk,
    createWirelessDevice_tags,
    createWirelessDevice_type,
    createWirelessDevice_destinationName,
    createWirelessDeviceResponse_arn,
    createWirelessDeviceResponse_id,
    createWirelessDeviceResponse_httpStatus,

    -- ** CreateWirelessGateway
    createWirelessGateway_clientRequestToken,
    createWirelessGateway_description,
    createWirelessGateway_name,
    createWirelessGateway_tags,
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
    createWirelessGatewayTaskDefinition_clientRequestToken,
    createWirelessGatewayTaskDefinition_name,
    createWirelessGatewayTaskDefinition_tags,
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

    -- ** DeleteFuotaTask
    deleteFuotaTask_id,
    deleteFuotaTaskResponse_httpStatus,

    -- ** DeleteMulticastGroup
    deleteMulticastGroup_id,
    deleteMulticastGroupResponse_httpStatus,

    -- ** DeleteNetworkAnalyzerConfiguration
    deleteNetworkAnalyzerConfiguration_configurationName,
    deleteNetworkAnalyzerConfigurationResponse_httpStatus,

    -- ** DeleteQueuedMessages
    deleteQueuedMessages_wirelessDeviceType,
    deleteQueuedMessages_id,
    deleteQueuedMessages_messageId,
    deleteQueuedMessagesResponse_httpStatus,

    -- ** DeleteServiceProfile
    deleteServiceProfile_id,
    deleteServiceProfileResponse_httpStatus,

    -- ** DeleteWirelessDevice
    deleteWirelessDevice_id,
    deleteWirelessDeviceResponse_httpStatus,

    -- ** DeleteWirelessDeviceImportTask
    deleteWirelessDeviceImportTask_id,
    deleteWirelessDeviceImportTaskResponse_httpStatus,

    -- ** DeleteWirelessGateway
    deleteWirelessGateway_id,
    deleteWirelessGatewayResponse_httpStatus,

    -- ** DeleteWirelessGatewayTask
    deleteWirelessGatewayTask_id,
    deleteWirelessGatewayTaskResponse_httpStatus,

    -- ** DeleteWirelessGatewayTaskDefinition
    deleteWirelessGatewayTaskDefinition_id,
    deleteWirelessGatewayTaskDefinitionResponse_httpStatus,

    -- ** DeregisterWirelessDevice
    deregisterWirelessDevice_wirelessDeviceType,
    deregisterWirelessDevice_identifier,
    deregisterWirelessDeviceResponse_httpStatus,

    -- ** DisassociateAwsAccountFromPartnerAccount
    disassociateAwsAccountFromPartnerAccount_partnerAccountId,
    disassociateAwsAccountFromPartnerAccount_partnerType,
    disassociateAwsAccountFromPartnerAccountResponse_httpStatus,

    -- ** DisassociateMulticastGroupFromFuotaTask
    disassociateMulticastGroupFromFuotaTask_id,
    disassociateMulticastGroupFromFuotaTask_multicastGroupId,
    disassociateMulticastGroupFromFuotaTaskResponse_httpStatus,

    -- ** DisassociateWirelessDeviceFromFuotaTask
    disassociateWirelessDeviceFromFuotaTask_id,
    disassociateWirelessDeviceFromFuotaTask_wirelessDeviceId,
    disassociateWirelessDeviceFromFuotaTaskResponse_httpStatus,

    -- ** DisassociateWirelessDeviceFromMulticastGroup
    disassociateWirelessDeviceFromMulticastGroup_id,
    disassociateWirelessDeviceFromMulticastGroup_wirelessDeviceId,
    disassociateWirelessDeviceFromMulticastGroupResponse_httpStatus,

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
    getDestinationResponse_arn,
    getDestinationResponse_description,
    getDestinationResponse_expression,
    getDestinationResponse_expressionType,
    getDestinationResponse_name,
    getDestinationResponse_roleArn,
    getDestinationResponse_httpStatus,

    -- ** GetDeviceProfile
    getDeviceProfile_id,
    getDeviceProfileResponse_arn,
    getDeviceProfileResponse_id,
    getDeviceProfileResponse_loRaWAN,
    getDeviceProfileResponse_name,
    getDeviceProfileResponse_sidewalk,
    getDeviceProfileResponse_httpStatus,

    -- ** GetEventConfigurationByResourceTypes
    getEventConfigurationByResourceTypesResponse_connectionStatus,
    getEventConfigurationByResourceTypesResponse_deviceRegistrationState,
    getEventConfigurationByResourceTypesResponse_join,
    getEventConfigurationByResourceTypesResponse_messageDeliveryStatus,
    getEventConfigurationByResourceTypesResponse_proximity,
    getEventConfigurationByResourceTypesResponse_httpStatus,

    -- ** GetFuotaTask
    getFuotaTask_id,
    getFuotaTaskResponse_arn,
    getFuotaTaskResponse_createdAt,
    getFuotaTaskResponse_description,
    getFuotaTaskResponse_firmwareUpdateImage,
    getFuotaTaskResponse_firmwareUpdateRole,
    getFuotaTaskResponse_fragmentIntervalMS,
    getFuotaTaskResponse_fragmentSizeBytes,
    getFuotaTaskResponse_id,
    getFuotaTaskResponse_loRaWAN,
    getFuotaTaskResponse_name,
    getFuotaTaskResponse_redundancyPercent,
    getFuotaTaskResponse_status,
    getFuotaTaskResponse_httpStatus,

    -- ** GetLogLevelsByResourceTypes
    getLogLevelsByResourceTypesResponse_defaultLogLevel,
    getLogLevelsByResourceTypesResponse_wirelessDeviceLogOptions,
    getLogLevelsByResourceTypesResponse_wirelessGatewayLogOptions,
    getLogLevelsByResourceTypesResponse_httpStatus,

    -- ** GetMulticastGroup
    getMulticastGroup_id,
    getMulticastGroupResponse_arn,
    getMulticastGroupResponse_createdAt,
    getMulticastGroupResponse_description,
    getMulticastGroupResponse_id,
    getMulticastGroupResponse_loRaWAN,
    getMulticastGroupResponse_name,
    getMulticastGroupResponse_status,
    getMulticastGroupResponse_httpStatus,

    -- ** GetMulticastGroupSession
    getMulticastGroupSession_id,
    getMulticastGroupSessionResponse_loRaWAN,
    getMulticastGroupSessionResponse_httpStatus,

    -- ** GetNetworkAnalyzerConfiguration
    getNetworkAnalyzerConfiguration_configurationName,
    getNetworkAnalyzerConfigurationResponse_arn,
    getNetworkAnalyzerConfigurationResponse_description,
    getNetworkAnalyzerConfigurationResponse_multicastGroups,
    getNetworkAnalyzerConfigurationResponse_name,
    getNetworkAnalyzerConfigurationResponse_traceContent,
    getNetworkAnalyzerConfigurationResponse_wirelessDevices,
    getNetworkAnalyzerConfigurationResponse_wirelessGateways,
    getNetworkAnalyzerConfigurationResponse_httpStatus,

    -- ** GetPartnerAccount
    getPartnerAccount_partnerAccountId,
    getPartnerAccount_partnerType,
    getPartnerAccountResponse_accountLinked,
    getPartnerAccountResponse_sidewalk,
    getPartnerAccountResponse_httpStatus,

    -- ** GetPositionEstimate
    getPositionEstimate_cellTowers,
    getPositionEstimate_gnss,
    getPositionEstimate_ip,
    getPositionEstimate_timestamp,
    getPositionEstimate_wiFiAccessPoints,
    getPositionEstimateResponse_geoJsonPayload,
    getPositionEstimateResponse_httpStatus,

    -- ** GetResourceEventConfiguration
    getResourceEventConfiguration_partnerType,
    getResourceEventConfiguration_identifier,
    getResourceEventConfiguration_identifierType,
    getResourceEventConfigurationResponse_connectionStatus,
    getResourceEventConfigurationResponse_deviceRegistrationState,
    getResourceEventConfigurationResponse_join,
    getResourceEventConfigurationResponse_messageDeliveryStatus,
    getResourceEventConfigurationResponse_proximity,
    getResourceEventConfigurationResponse_httpStatus,

    -- ** GetResourceLogLevel
    getResourceLogLevel_resourceIdentifier,
    getResourceLogLevel_resourceType,
    getResourceLogLevelResponse_logLevel,
    getResourceLogLevelResponse_httpStatus,

    -- ** GetResourcePosition
    getResourcePosition_resourceIdentifier,
    getResourcePosition_resourceType,
    getResourcePositionResponse_geoJsonPayload,
    getResourcePositionResponse_httpStatus,

    -- ** GetServiceEndpoint
    getServiceEndpoint_serviceType,
    getServiceEndpointResponse_serverTrust,
    getServiceEndpointResponse_serviceEndpoint,
    getServiceEndpointResponse_serviceType,
    getServiceEndpointResponse_httpStatus,

    -- ** GetServiceProfile
    getServiceProfile_id,
    getServiceProfileResponse_arn,
    getServiceProfileResponse_id,
    getServiceProfileResponse_loRaWAN,
    getServiceProfileResponse_name,
    getServiceProfileResponse_httpStatus,

    -- ** GetWirelessDevice
    getWirelessDevice_identifier,
    getWirelessDevice_identifierType,
    getWirelessDeviceResponse_arn,
    getWirelessDeviceResponse_description,
    getWirelessDeviceResponse_destinationName,
    getWirelessDeviceResponse_id,
    getWirelessDeviceResponse_loRaWAN,
    getWirelessDeviceResponse_name,
    getWirelessDeviceResponse_positioning,
    getWirelessDeviceResponse_sidewalk,
    getWirelessDeviceResponse_thingArn,
    getWirelessDeviceResponse_thingName,
    getWirelessDeviceResponse_type,
    getWirelessDeviceResponse_httpStatus,

    -- ** GetWirelessDeviceImportTask
    getWirelessDeviceImportTask_id,
    getWirelessDeviceImportTaskResponse_arn,
    getWirelessDeviceImportTaskResponse_creationTime,
    getWirelessDeviceImportTaskResponse_destinationName,
    getWirelessDeviceImportTaskResponse_failedImportedDeviceCount,
    getWirelessDeviceImportTaskResponse_id,
    getWirelessDeviceImportTaskResponse_initializedImportedDeviceCount,
    getWirelessDeviceImportTaskResponse_onboardedImportedDeviceCount,
    getWirelessDeviceImportTaskResponse_pendingImportedDeviceCount,
    getWirelessDeviceImportTaskResponse_sidewalk,
    getWirelessDeviceImportTaskResponse_status,
    getWirelessDeviceImportTaskResponse_statusReason,
    getWirelessDeviceImportTaskResponse_httpStatus,

    -- ** GetWirelessDeviceStatistics
    getWirelessDeviceStatistics_wirelessDeviceId,
    getWirelessDeviceStatisticsResponse_lastUplinkReceivedAt,
    getWirelessDeviceStatisticsResponse_loRaWAN,
    getWirelessDeviceStatisticsResponse_sidewalk,
    getWirelessDeviceStatisticsResponse_wirelessDeviceId,
    getWirelessDeviceStatisticsResponse_httpStatus,

    -- ** GetWirelessGateway
    getWirelessGateway_identifier,
    getWirelessGateway_identifierType,
    getWirelessGatewayResponse_arn,
    getWirelessGatewayResponse_description,
    getWirelessGatewayResponse_id,
    getWirelessGatewayResponse_loRaWAN,
    getWirelessGatewayResponse_name,
    getWirelessGatewayResponse_thingArn,
    getWirelessGatewayResponse_thingName,
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
    getWirelessGatewayStatisticsResponse_connectionStatus,
    getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt,
    getWirelessGatewayStatisticsResponse_wirelessGatewayId,
    getWirelessGatewayStatisticsResponse_httpStatus,

    -- ** GetWirelessGatewayTask
    getWirelessGatewayTask_id,
    getWirelessGatewayTaskResponse_lastUplinkReceivedAt,
    getWirelessGatewayTaskResponse_status,
    getWirelessGatewayTaskResponse_taskCreatedAt,
    getWirelessGatewayTaskResponse_wirelessGatewayId,
    getWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId,
    getWirelessGatewayTaskResponse_httpStatus,

    -- ** GetWirelessGatewayTaskDefinition
    getWirelessGatewayTaskDefinition_id,
    getWirelessGatewayTaskDefinitionResponse_arn,
    getWirelessGatewayTaskDefinitionResponse_autoCreateTasks,
    getWirelessGatewayTaskDefinitionResponse_name,
    getWirelessGatewayTaskDefinitionResponse_update,
    getWirelessGatewayTaskDefinitionResponse_httpStatus,

    -- ** ListDestinations
    listDestinations_maxResults,
    listDestinations_nextToken,
    listDestinationsResponse_destinationList,
    listDestinationsResponse_nextToken,
    listDestinationsResponse_httpStatus,

    -- ** ListDeviceProfiles
    listDeviceProfiles_deviceProfileType,
    listDeviceProfiles_maxResults,
    listDeviceProfiles_nextToken,
    listDeviceProfilesResponse_deviceProfileList,
    listDeviceProfilesResponse_nextToken,
    listDeviceProfilesResponse_httpStatus,

    -- ** ListDevicesForWirelessDeviceImportTask
    listDevicesForWirelessDeviceImportTask_maxResults,
    listDevicesForWirelessDeviceImportTask_nextToken,
    listDevicesForWirelessDeviceImportTask_status,
    listDevicesForWirelessDeviceImportTask_id,
    listDevicesForWirelessDeviceImportTaskResponse_destinationName,
    listDevicesForWirelessDeviceImportTaskResponse_importedWirelessDeviceList,
    listDevicesForWirelessDeviceImportTaskResponse_nextToken,
    listDevicesForWirelessDeviceImportTaskResponse_httpStatus,

    -- ** ListEventConfigurations
    listEventConfigurations_maxResults,
    listEventConfigurations_nextToken,
    listEventConfigurations_resourceType,
    listEventConfigurationsResponse_eventConfigurationsList,
    listEventConfigurationsResponse_nextToken,
    listEventConfigurationsResponse_httpStatus,

    -- ** ListFuotaTasks
    listFuotaTasks_maxResults,
    listFuotaTasks_nextToken,
    listFuotaTasksResponse_fuotaTaskList,
    listFuotaTasksResponse_nextToken,
    listFuotaTasksResponse_httpStatus,

    -- ** ListMulticastGroups
    listMulticastGroups_maxResults,
    listMulticastGroups_nextToken,
    listMulticastGroupsResponse_multicastGroupList,
    listMulticastGroupsResponse_nextToken,
    listMulticastGroupsResponse_httpStatus,

    -- ** ListMulticastGroupsByFuotaTask
    listMulticastGroupsByFuotaTask_maxResults,
    listMulticastGroupsByFuotaTask_nextToken,
    listMulticastGroupsByFuotaTask_id,
    listMulticastGroupsByFuotaTaskResponse_multicastGroupList,
    listMulticastGroupsByFuotaTaskResponse_nextToken,
    listMulticastGroupsByFuotaTaskResponse_httpStatus,

    -- ** ListNetworkAnalyzerConfigurations
    listNetworkAnalyzerConfigurations_maxResults,
    listNetworkAnalyzerConfigurations_nextToken,
    listNetworkAnalyzerConfigurationsResponse_networkAnalyzerConfigurationList,
    listNetworkAnalyzerConfigurationsResponse_nextToken,
    listNetworkAnalyzerConfigurationsResponse_httpStatus,

    -- ** ListPartnerAccounts
    listPartnerAccounts_maxResults,
    listPartnerAccounts_nextToken,
    listPartnerAccountsResponse_nextToken,
    listPartnerAccountsResponse_sidewalk,
    listPartnerAccountsResponse_httpStatus,

    -- ** ListQueuedMessages
    listQueuedMessages_maxResults,
    listQueuedMessages_nextToken,
    listQueuedMessages_wirelessDeviceType,
    listQueuedMessages_id,
    listQueuedMessagesResponse_downlinkQueueMessagesList,
    listQueuedMessagesResponse_nextToken,
    listQueuedMessagesResponse_httpStatus,

    -- ** ListServiceProfiles
    listServiceProfiles_maxResults,
    listServiceProfiles_nextToken,
    listServiceProfilesResponse_nextToken,
    listServiceProfilesResponse_serviceProfileList,
    listServiceProfilesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWirelessDeviceImportTasks
    listWirelessDeviceImportTasks_maxResults,
    listWirelessDeviceImportTasks_nextToken,
    listWirelessDeviceImportTasksResponse_nextToken,
    listWirelessDeviceImportTasksResponse_wirelessDeviceImportTaskList,
    listWirelessDeviceImportTasksResponse_httpStatus,

    -- ** ListWirelessDevices
    listWirelessDevices_destinationName,
    listWirelessDevices_deviceProfileId,
    listWirelessDevices_fuotaTaskId,
    listWirelessDevices_maxResults,
    listWirelessDevices_multicastGroupId,
    listWirelessDevices_nextToken,
    listWirelessDevices_serviceProfileId,
    listWirelessDevices_wirelessDeviceType,
    listWirelessDevicesResponse_nextToken,
    listWirelessDevicesResponse_wirelessDeviceList,
    listWirelessDevicesResponse_httpStatus,

    -- ** ListWirelessGatewayTaskDefinitions
    listWirelessGatewayTaskDefinitions_maxResults,
    listWirelessGatewayTaskDefinitions_nextToken,
    listWirelessGatewayTaskDefinitions_taskDefinitionType,
    listWirelessGatewayTaskDefinitionsResponse_nextToken,
    listWirelessGatewayTaskDefinitionsResponse_taskDefinitions,
    listWirelessGatewayTaskDefinitionsResponse_httpStatus,

    -- ** ListWirelessGateways
    listWirelessGateways_maxResults,
    listWirelessGateways_nextToken,
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

    -- ** SendDataToMulticastGroup
    sendDataToMulticastGroup_id,
    sendDataToMulticastGroup_payloadData,
    sendDataToMulticastGroup_wirelessMetadata,
    sendDataToMulticastGroupResponse_messageId,
    sendDataToMulticastGroupResponse_httpStatus,

    -- ** SendDataToWirelessDevice
    sendDataToWirelessDevice_wirelessMetadata,
    sendDataToWirelessDevice_id,
    sendDataToWirelessDevice_transmitMode,
    sendDataToWirelessDevice_payloadData,
    sendDataToWirelessDeviceResponse_messageId,
    sendDataToWirelessDeviceResponse_httpStatus,

    -- ** StartBulkAssociateWirelessDeviceWithMulticastGroup
    startBulkAssociateWirelessDeviceWithMulticastGroup_queryString,
    startBulkAssociateWirelessDeviceWithMulticastGroup_tags,
    startBulkAssociateWirelessDeviceWithMulticastGroup_id,
    startBulkAssociateWirelessDeviceWithMulticastGroupResponse_httpStatus,

    -- ** StartBulkDisassociateWirelessDeviceFromMulticastGroup
    startBulkDisassociateWirelessDeviceFromMulticastGroup_queryString,
    startBulkDisassociateWirelessDeviceFromMulticastGroup_tags,
    startBulkDisassociateWirelessDeviceFromMulticastGroup_id,
    startBulkDisassociateWirelessDeviceFromMulticastGroupResponse_httpStatus,

    -- ** StartFuotaTask
    startFuotaTask_loRaWAN,
    startFuotaTask_id,
    startFuotaTaskResponse_httpStatus,

    -- ** StartMulticastGroupSession
    startMulticastGroupSession_id,
    startMulticastGroupSession_loRaWAN,
    startMulticastGroupSessionResponse_httpStatus,

    -- ** StartSingleWirelessDeviceImportTask
    startSingleWirelessDeviceImportTask_clientRequestToken,
    startSingleWirelessDeviceImportTask_deviceName,
    startSingleWirelessDeviceImportTask_tags,
    startSingleWirelessDeviceImportTask_destinationName,
    startSingleWirelessDeviceImportTask_sidewalk,
    startSingleWirelessDeviceImportTaskResponse_arn,
    startSingleWirelessDeviceImportTaskResponse_id,
    startSingleWirelessDeviceImportTaskResponse_httpStatus,

    -- ** StartWirelessDeviceImportTask
    startWirelessDeviceImportTask_clientRequestToken,
    startWirelessDeviceImportTask_tags,
    startWirelessDeviceImportTask_destinationName,
    startWirelessDeviceImportTask_sidewalk,
    startWirelessDeviceImportTaskResponse_arn,
    startWirelessDeviceImportTaskResponse_id,
    startWirelessDeviceImportTaskResponse_httpStatus,

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
    updateDestination_description,
    updateDestination_expression,
    updateDestination_expressionType,
    updateDestination_roleArn,
    updateDestination_name,
    updateDestinationResponse_httpStatus,

    -- ** UpdateEventConfigurationByResourceTypes
    updateEventConfigurationByResourceTypes_connectionStatus,
    updateEventConfigurationByResourceTypes_deviceRegistrationState,
    updateEventConfigurationByResourceTypes_join,
    updateEventConfigurationByResourceTypes_messageDeliveryStatus,
    updateEventConfigurationByResourceTypes_proximity,
    updateEventConfigurationByResourceTypesResponse_httpStatus,

    -- ** UpdateFuotaTask
    updateFuotaTask_description,
    updateFuotaTask_firmwareUpdateImage,
    updateFuotaTask_firmwareUpdateRole,
    updateFuotaTask_fragmentIntervalMS,
    updateFuotaTask_fragmentSizeBytes,
    updateFuotaTask_loRaWAN,
    updateFuotaTask_name,
    updateFuotaTask_redundancyPercent,
    updateFuotaTask_id,
    updateFuotaTaskResponse_httpStatus,

    -- ** UpdateLogLevelsByResourceTypes
    updateLogLevelsByResourceTypes_defaultLogLevel,
    updateLogLevelsByResourceTypes_wirelessDeviceLogOptions,
    updateLogLevelsByResourceTypes_wirelessGatewayLogOptions,
    updateLogLevelsByResourceTypesResponse_httpStatus,

    -- ** UpdateMulticastGroup
    updateMulticastGroup_description,
    updateMulticastGroup_loRaWAN,
    updateMulticastGroup_name,
    updateMulticastGroup_id,
    updateMulticastGroupResponse_httpStatus,

    -- ** UpdateNetworkAnalyzerConfiguration
    updateNetworkAnalyzerConfiguration_description,
    updateNetworkAnalyzerConfiguration_multicastGroupsToAdd,
    updateNetworkAnalyzerConfiguration_multicastGroupsToRemove,
    updateNetworkAnalyzerConfiguration_traceContent,
    updateNetworkAnalyzerConfiguration_wirelessDevicesToAdd,
    updateNetworkAnalyzerConfiguration_wirelessDevicesToRemove,
    updateNetworkAnalyzerConfiguration_wirelessGatewaysToAdd,
    updateNetworkAnalyzerConfiguration_wirelessGatewaysToRemove,
    updateNetworkAnalyzerConfiguration_configurationName,
    updateNetworkAnalyzerConfigurationResponse_httpStatus,

    -- ** UpdatePartnerAccount
    updatePartnerAccount_sidewalk,
    updatePartnerAccount_partnerAccountId,
    updatePartnerAccount_partnerType,
    updatePartnerAccountResponse_httpStatus,

    -- ** UpdateResourceEventConfiguration
    updateResourceEventConfiguration_connectionStatus,
    updateResourceEventConfiguration_deviceRegistrationState,
    updateResourceEventConfiguration_join,
    updateResourceEventConfiguration_messageDeliveryStatus,
    updateResourceEventConfiguration_partnerType,
    updateResourceEventConfiguration_proximity,
    updateResourceEventConfiguration_identifier,
    updateResourceEventConfiguration_identifierType,
    updateResourceEventConfigurationResponse_httpStatus,

    -- ** UpdateResourcePosition
    updateResourcePosition_geoJsonPayload,
    updateResourcePosition_resourceIdentifier,
    updateResourcePosition_resourceType,
    updateResourcePositionResponse_httpStatus,

    -- ** UpdateWirelessDevice
    updateWirelessDevice_description,
    updateWirelessDevice_destinationName,
    updateWirelessDevice_loRaWAN,
    updateWirelessDevice_name,
    updateWirelessDevice_positioning,
    updateWirelessDevice_id,
    updateWirelessDeviceResponse_httpStatus,

    -- ** UpdateWirelessDeviceImportTask
    updateWirelessDeviceImportTask_id,
    updateWirelessDeviceImportTask_sidewalk,
    updateWirelessDeviceImportTaskResponse_httpStatus,

    -- ** UpdateWirelessGateway
    updateWirelessGateway_description,
    updateWirelessGateway_joinEuiFilters,
    updateWirelessGateway_maxEirp,
    updateWirelessGateway_name,
    updateWirelessGateway_netIdFilters,
    updateWirelessGateway_id,
    updateWirelessGatewayResponse_httpStatus,

    -- * Types

    -- ** AbpV1_0_x
    abpV1_0_x_devAddr,
    abpV1_0_x_fCntStart,
    abpV1_0_x_sessionKeys,

    -- ** AbpV1_1
    abpV1_1_devAddr,
    abpV1_1_fCntStart,
    abpV1_1_sessionKeys,

    -- ** ApplicationConfig
    applicationConfig_destinationName,
    applicationConfig_fPort,
    applicationConfig_type,

    -- ** Beaconing
    beaconing_dataRate,
    beaconing_frequencies,

    -- ** CdmaLocalId
    cdmaLocalId_pnOffset,
    cdmaLocalId_cdmaChannel,

    -- ** CdmaNmrObj
    cdmaNmrObj_baseStationId,
    cdmaNmrObj_pilotPower,
    cdmaNmrObj_pnOffset,
    cdmaNmrObj_cdmaChannel,

    -- ** CdmaObj
    cdmaObj_baseLat,
    cdmaObj_baseLng,
    cdmaObj_cdmaLocalId,
    cdmaObj_cdmaNmr,
    cdmaObj_pilotPower,
    cdmaObj_registrationZone,
    cdmaObj_systemId,
    cdmaObj_networkId,
    cdmaObj_baseStationId,

    -- ** CellTowers
    cellTowers_cdma,
    cellTowers_gsm,
    cellTowers_lte,
    cellTowers_tdscdma,
    cellTowers_wcdma,

    -- ** CertificateList
    certificateList_signingAlg,
    certificateList_value,

    -- ** ConnectionStatusEventConfiguration
    connectionStatusEventConfiguration_loRaWAN,
    connectionStatusEventConfiguration_wirelessGatewayIdEventTopic,

    -- ** ConnectionStatusResourceTypeEventConfiguration
    connectionStatusResourceTypeEventConfiguration_loRaWAN,

    -- ** DakCertificateMetadata
    dakCertificateMetadata_apId,
    dakCertificateMetadata_deviceTypeId,
    dakCertificateMetadata_factorySupport,
    dakCertificateMetadata_maxAllowedSignature,
    dakCertificateMetadata_certificateId,

    -- ** Destinations
    destinations_arn,
    destinations_description,
    destinations_expression,
    destinations_expressionType,
    destinations_name,
    destinations_roleArn,

    -- ** DeviceProfile
    deviceProfile_arn,
    deviceProfile_id,
    deviceProfile_name,

    -- ** DeviceRegistrationStateEventConfiguration
    deviceRegistrationStateEventConfiguration_sidewalk,
    deviceRegistrationStateEventConfiguration_wirelessDeviceIdEventTopic,

    -- ** DeviceRegistrationStateResourceTypeEventConfiguration
    deviceRegistrationStateResourceTypeEventConfiguration_sidewalk,

    -- ** DownlinkQueueMessage
    downlinkQueueMessage_loRaWAN,
    downlinkQueueMessage_messageId,
    downlinkQueueMessage_receivedAt,
    downlinkQueueMessage_transmitMode,

    -- ** EventConfigurationItem
    eventConfigurationItem_events,
    eventConfigurationItem_identifier,
    eventConfigurationItem_identifierType,
    eventConfigurationItem_partnerType,

    -- ** EventNotificationItemConfigurations
    eventNotificationItemConfigurations_connectionStatus,
    eventNotificationItemConfigurations_deviceRegistrationState,
    eventNotificationItemConfigurations_join,
    eventNotificationItemConfigurations_messageDeliveryStatus,
    eventNotificationItemConfigurations_proximity,

    -- ** FPorts
    fPorts_applications,
    fPorts_clockSync,
    fPorts_fuota,
    fPorts_multicast,
    fPorts_positioning,

    -- ** FuotaTask
    fuotaTask_arn,
    fuotaTask_id,
    fuotaTask_name,

    -- ** GatewayListItem
    gatewayListItem_gatewayId,
    gatewayListItem_downlinkFrequency,

    -- ** GlobalIdentity
    globalIdentity_lac,
    globalIdentity_geranCid,

    -- ** Gnss
    gnss_assistAltitude,
    gnss_assistPosition,
    gnss_captureTime,
    gnss_captureTimeAccuracy,
    gnss_use2DSolver,
    gnss_payload,

    -- ** GsmLocalId
    gsmLocalId_bsic,
    gsmLocalId_bcch,

    -- ** GsmNmrObj
    gsmNmrObj_globalIdentity,
    gsmNmrObj_rxLevel,
    gsmNmrObj_bsic,
    gsmNmrObj_bcch,

    -- ** GsmObj
    gsmObj_gsmLocalId,
    gsmObj_gsmNmr,
    gsmObj_gsmTimingAdvance,
    gsmObj_rxLevel,
    gsmObj_mcc,
    gsmObj_mnc,
    gsmObj_lac,
    gsmObj_geranCid,

    -- ** ImportedSidewalkDevice
    importedSidewalkDevice_lastUpdateTime,
    importedSidewalkDevice_onboardingStatus,
    importedSidewalkDevice_onboardingStatusReason,
    importedSidewalkDevice_sidewalkManufacturingSn,

    -- ** ImportedWirelessDevice
    importedWirelessDevice_sidewalk,

    -- ** Ip
    ip_ipAddress,

    -- ** JoinEventConfiguration
    joinEventConfiguration_loRaWAN,
    joinEventConfiguration_wirelessDeviceIdEventTopic,

    -- ** JoinResourceTypeEventConfiguration
    joinResourceTypeEventConfiguration_loRaWAN,

    -- ** LoRaWANConnectionStatusEventNotificationConfigurations
    loRaWANConnectionStatusEventNotificationConfigurations_gatewayEuiEventTopic,

    -- ** LoRaWANConnectionStatusResourceTypeEventConfiguration
    loRaWANConnectionStatusResourceTypeEventConfiguration_wirelessGatewayEventTopic,

    -- ** LoRaWANDevice
    loRaWANDevice_abpV1_0_x,
    loRaWANDevice_abpV1_1,
    loRaWANDevice_devEui,
    loRaWANDevice_deviceProfileId,
    loRaWANDevice_fPorts,
    loRaWANDevice_otaaV1_0_x,
    loRaWANDevice_otaaV1_1,
    loRaWANDevice_serviceProfileId,

    -- ** LoRaWANDeviceMetadata
    loRaWANDeviceMetadata_dataRate,
    loRaWANDeviceMetadata_devEui,
    loRaWANDeviceMetadata_fPort,
    loRaWANDeviceMetadata_frequency,
    loRaWANDeviceMetadata_gateways,
    loRaWANDeviceMetadata_timestamp,

    -- ** LoRaWANDeviceProfile
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

    -- ** LoRaWANFuotaTask
    loRaWANFuotaTask_rfRegion,

    -- ** LoRaWANFuotaTaskGetInfo
    loRaWANFuotaTaskGetInfo_rfRegion,
    loRaWANFuotaTaskGetInfo_startTime,

    -- ** LoRaWANGateway
    loRaWANGateway_beaconing,
    loRaWANGateway_gatewayEui,
    loRaWANGateway_joinEuiFilters,
    loRaWANGateway_maxEirp,
    loRaWANGateway_netIdFilters,
    loRaWANGateway_rfRegion,
    loRaWANGateway_subBands,

    -- ** LoRaWANGatewayCurrentVersion
    loRaWANGatewayCurrentVersion_currentVersion,

    -- ** LoRaWANGatewayMetadata
    loRaWANGatewayMetadata_gatewayEui,
    loRaWANGatewayMetadata_rssi,
    loRaWANGatewayMetadata_snr,

    -- ** LoRaWANGatewayVersion
    loRaWANGatewayVersion_model,
    loRaWANGatewayVersion_packageVersion,
    loRaWANGatewayVersion_station,

    -- ** LoRaWANGetServiceProfileInfo
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

    -- ** LoRaWANJoinEventNotificationConfigurations
    loRaWANJoinEventNotificationConfigurations_devEuiEventTopic,

    -- ** LoRaWANJoinResourceTypeEventConfiguration
    loRaWANJoinResourceTypeEventConfiguration_wirelessDeviceEventTopic,

    -- ** LoRaWANListDevice
    loRaWANListDevice_devEui,

    -- ** LoRaWANMulticast
    loRaWANMulticast_dlClass,
    loRaWANMulticast_rfRegion,

    -- ** LoRaWANMulticastGet
    loRaWANMulticastGet_dlClass,
    loRaWANMulticastGet_numberOfDevicesInGroup,
    loRaWANMulticastGet_numberOfDevicesRequested,
    loRaWANMulticastGet_rfRegion,

    -- ** LoRaWANMulticastMetadata
    loRaWANMulticastMetadata_fPort,

    -- ** LoRaWANMulticastSession
    loRaWANMulticastSession_dlDr,
    loRaWANMulticastSession_dlFreq,
    loRaWANMulticastSession_pingSlotPeriod,
    loRaWANMulticastSession_sessionStartTime,
    loRaWANMulticastSession_sessionTimeout,

    -- ** LoRaWANSendDataToDevice
    loRaWANSendDataToDevice_fPort,
    loRaWANSendDataToDevice_participatingGateways,

    -- ** LoRaWANServiceProfile
    loRaWANServiceProfile_addGwMetadata,
    loRaWANServiceProfile_drMax,
    loRaWANServiceProfile_drMin,
    loRaWANServiceProfile_prAllowed,
    loRaWANServiceProfile_raAllowed,

    -- ** LoRaWANStartFuotaTask
    loRaWANStartFuotaTask_startTime,

    -- ** LoRaWANUpdateDevice
    loRaWANUpdateDevice_abpV1_0_x,
    loRaWANUpdateDevice_abpV1_1,
    loRaWANUpdateDevice_deviceProfileId,
    loRaWANUpdateDevice_fPorts,
    loRaWANUpdateDevice_serviceProfileId,

    -- ** LoRaWANUpdateGatewayTaskCreate
    loRaWANUpdateGatewayTaskCreate_currentVersion,
    loRaWANUpdateGatewayTaskCreate_sigKeyCrc,
    loRaWANUpdateGatewayTaskCreate_updateSignature,
    loRaWANUpdateGatewayTaskCreate_updateVersion,

    -- ** LoRaWANUpdateGatewayTaskEntry
    loRaWANUpdateGatewayTaskEntry_currentVersion,
    loRaWANUpdateGatewayTaskEntry_updateVersion,

    -- ** LteLocalId
    lteLocalId_pci,
    lteLocalId_earfcn,

    -- ** LteNmrObj
    lteNmrObj_rsrp,
    lteNmrObj_rsrq,
    lteNmrObj_pci,
    lteNmrObj_earfcn,
    lteNmrObj_eutranCid,

    -- ** LteObj
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

    -- ** MessageDeliveryStatusEventConfiguration
    messageDeliveryStatusEventConfiguration_sidewalk,
    messageDeliveryStatusEventConfiguration_wirelessDeviceIdEventTopic,

    -- ** MessageDeliveryStatusResourceTypeEventConfiguration
    messageDeliveryStatusResourceTypeEventConfiguration_sidewalk,

    -- ** MulticastGroup
    multicastGroup_arn,
    multicastGroup_id,
    multicastGroup_name,

    -- ** MulticastGroupByFuotaTask
    multicastGroupByFuotaTask_id,

    -- ** MulticastWirelessMetadata
    multicastWirelessMetadata_loRaWAN,

    -- ** NetworkAnalyzerConfigurations
    networkAnalyzerConfigurations_arn,
    networkAnalyzerConfigurations_name,

    -- ** OtaaV1_0_x
    otaaV1_0_x_appEui,
    otaaV1_0_x_appKey,
    otaaV1_0_x_genAppKey,

    -- ** OtaaV1_1
    otaaV1_1_appKey,
    otaaV1_1_joinEui,
    otaaV1_1_nwkKey,

    -- ** ParticipatingGateways
    participatingGateways_downlinkMode,
    participatingGateways_gatewayList,
    participatingGateways_transmissionInterval,

    -- ** Positioning
    positioning_clockSync,
    positioning_gnss,
    positioning_stream,

    -- ** ProximityEventConfiguration
    proximityEventConfiguration_sidewalk,
    proximityEventConfiguration_wirelessDeviceIdEventTopic,

    -- ** ProximityResourceTypeEventConfiguration
    proximityResourceTypeEventConfiguration_sidewalk,

    -- ** ServiceProfile
    serviceProfile_arn,
    serviceProfile_id,
    serviceProfile_name,

    -- ** SessionKeysAbpV1_0_x
    sessionKeysAbpV1_0_x_appSKey,
    sessionKeysAbpV1_0_x_nwkSKey,

    -- ** SessionKeysAbpV1_1
    sessionKeysAbpV1_1_appSKey,
    sessionKeysAbpV1_1_fNwkSIntKey,
    sessionKeysAbpV1_1_nwkSEncKey,
    sessionKeysAbpV1_1_sNwkSIntKey,

    -- ** SidewalkAccountInfo
    sidewalkAccountInfo_amazonId,
    sidewalkAccountInfo_appServerPrivateKey,

    -- ** SidewalkAccountInfoWithFingerprint
    sidewalkAccountInfoWithFingerprint_amazonId,
    sidewalkAccountInfoWithFingerprint_arn,
    sidewalkAccountInfoWithFingerprint_fingerprint,

    -- ** SidewalkCreateDeviceProfile

    -- ** SidewalkCreateWirelessDevice
    sidewalkCreateWirelessDevice_deviceProfileId,

    -- ** SidewalkDevice
    sidewalkDevice_amazonId,
    sidewalkDevice_certificateId,
    sidewalkDevice_deviceCertificates,
    sidewalkDevice_deviceProfileId,
    sidewalkDevice_privateKeys,
    sidewalkDevice_sidewalkId,
    sidewalkDevice_sidewalkManufacturingSn,
    sidewalkDevice_status,

    -- ** SidewalkDeviceMetadata
    sidewalkDeviceMetadata_batteryLevel,
    sidewalkDeviceMetadata_deviceState,
    sidewalkDeviceMetadata_event,
    sidewalkDeviceMetadata_rssi,

    -- ** SidewalkEventNotificationConfigurations
    sidewalkEventNotificationConfigurations_amazonIdEventTopic,

    -- ** SidewalkGetDeviceProfile
    sidewalkGetDeviceProfile_applicationServerPublicKey,
    sidewalkGetDeviceProfile_dakCertificateMetadata,
    sidewalkGetDeviceProfile_qualificationStatus,

    -- ** SidewalkGetStartImportInfo
    sidewalkGetStartImportInfo_deviceCreationFileList,
    sidewalkGetStartImportInfo_role,

    -- ** SidewalkListDevice
    sidewalkListDevice_amazonId,
    sidewalkListDevice_deviceCertificates,
    sidewalkListDevice_deviceProfileId,
    sidewalkListDevice_sidewalkId,
    sidewalkListDevice_sidewalkManufacturingSn,
    sidewalkListDevice_status,

    -- ** SidewalkResourceTypeEventConfiguration
    sidewalkResourceTypeEventConfiguration_wirelessDeviceEventTopic,

    -- ** SidewalkSendDataToDevice
    sidewalkSendDataToDevice_ackModeRetryDurationSecs,
    sidewalkSendDataToDevice_messageType,
    sidewalkSendDataToDevice_seq,

    -- ** SidewalkSingleStartImportInfo
    sidewalkSingleStartImportInfo_sidewalkManufacturingSn,

    -- ** SidewalkStartImportInfo
    sidewalkStartImportInfo_deviceCreationFile,
    sidewalkStartImportInfo_role,

    -- ** SidewalkUpdateAccount
    sidewalkUpdateAccount_appServerPrivateKey,

    -- ** SidewalkUpdateImportInfo
    sidewalkUpdateImportInfo_deviceCreationFile,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TdscdmaLocalId
    tdscdmaLocalId_uarfcn,
    tdscdmaLocalId_cellParams,

    -- ** TdscdmaNmrObj
    tdscdmaNmrObj_pathLoss,
    tdscdmaNmrObj_rscp,
    tdscdmaNmrObj_utranCid,
    tdscdmaNmrObj_uarfcn,
    tdscdmaNmrObj_cellParams,

    -- ** TdscdmaObj
    tdscdmaObj_lac,
    tdscdmaObj_pathLoss,
    tdscdmaObj_rscp,
    tdscdmaObj_tdscdmaLocalId,
    tdscdmaObj_tdscdmaNmr,
    tdscdmaObj_tdscdmaTimingAdvance,
    tdscdmaObj_mcc,
    tdscdmaObj_mnc,
    tdscdmaObj_utranCid,

    -- ** TraceContent
    traceContent_logLevel,
    traceContent_multicastFrameInfo,
    traceContent_wirelessDeviceFrameInfo,

    -- ** UpdateAbpV1_0_x
    updateAbpV1_0_x_fCntStart,

    -- ** UpdateAbpV1_1
    updateAbpV1_1_fCntStart,

    -- ** UpdateFPorts
    updateFPorts_applications,
    updateFPorts_positioning,

    -- ** UpdateWirelessGatewayTaskCreate
    updateWirelessGatewayTaskCreate_loRaWAN,
    updateWirelessGatewayTaskCreate_updateDataRole,
    updateWirelessGatewayTaskCreate_updateDataSource,

    -- ** UpdateWirelessGatewayTaskEntry
    updateWirelessGatewayTaskEntry_arn,
    updateWirelessGatewayTaskEntry_id,
    updateWirelessGatewayTaskEntry_loRaWAN,

    -- ** WcdmaLocalId
    wcdmaLocalId_uarfcndl,
    wcdmaLocalId_psc,

    -- ** WcdmaNmrObj
    wcdmaNmrObj_pathLoss,
    wcdmaNmrObj_rscp,
    wcdmaNmrObj_uarfcndl,
    wcdmaNmrObj_psc,
    wcdmaNmrObj_utranCid,

    -- ** WcdmaObj
    wcdmaObj_lac,
    wcdmaObj_pathLoss,
    wcdmaObj_rscp,
    wcdmaObj_wcdmaLocalId,
    wcdmaObj_wcdmaNmr,
    wcdmaObj_mcc,
    wcdmaObj_mnc,
    wcdmaObj_utranCid,

    -- ** WiFiAccessPoint
    wiFiAccessPoint_macAddress,
    wiFiAccessPoint_rss,

    -- ** WirelessDeviceEventLogOption
    wirelessDeviceEventLogOption_event,
    wirelessDeviceEventLogOption_logLevel,

    -- ** WirelessDeviceImportTask
    wirelessDeviceImportTask_arn,
    wirelessDeviceImportTask_creationTime,
    wirelessDeviceImportTask_destinationName,
    wirelessDeviceImportTask_failedImportedDeviceCount,
    wirelessDeviceImportTask_id,
    wirelessDeviceImportTask_initializedImportedDeviceCount,
    wirelessDeviceImportTask_onboardedImportedDeviceCount,
    wirelessDeviceImportTask_pendingImportedDeviceCount,
    wirelessDeviceImportTask_sidewalk,
    wirelessDeviceImportTask_status,
    wirelessDeviceImportTask_statusReason,

    -- ** WirelessDeviceLogOption
    wirelessDeviceLogOption_events,
    wirelessDeviceLogOption_type,
    wirelessDeviceLogOption_logLevel,

    -- ** WirelessDeviceStatistics
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

    -- ** WirelessGatewayEventLogOption
    wirelessGatewayEventLogOption_event,
    wirelessGatewayEventLogOption_logLevel,

    -- ** WirelessGatewayLogOption
    wirelessGatewayLogOption_events,
    wirelessGatewayLogOption_type,
    wirelessGatewayLogOption_logLevel,

    -- ** WirelessGatewayStatistics
    wirelessGatewayStatistics_arn,
    wirelessGatewayStatistics_description,
    wirelessGatewayStatistics_id,
    wirelessGatewayStatistics_lastUplinkReceivedAt,
    wirelessGatewayStatistics_loRaWAN,
    wirelessGatewayStatistics_name,

    -- ** WirelessMetadata
    wirelessMetadata_loRaWAN,
    wirelessMetadata_sidewalk,
  )
where

import Amazonka.IoTWireless.AssociateAwsAccountWithPartnerAccount
import Amazonka.IoTWireless.AssociateMulticastGroupWithFuotaTask
import Amazonka.IoTWireless.AssociateWirelessDeviceWithFuotaTask
import Amazonka.IoTWireless.AssociateWirelessDeviceWithMulticastGroup
import Amazonka.IoTWireless.AssociateWirelessDeviceWithThing
import Amazonka.IoTWireless.AssociateWirelessGatewayWithCertificate
import Amazonka.IoTWireless.AssociateWirelessGatewayWithThing
import Amazonka.IoTWireless.CancelMulticastGroupSession
import Amazonka.IoTWireless.CreateDestination
import Amazonka.IoTWireless.CreateDeviceProfile
import Amazonka.IoTWireless.CreateFuotaTask
import Amazonka.IoTWireless.CreateMulticastGroup
import Amazonka.IoTWireless.CreateNetworkAnalyzerConfiguration
import Amazonka.IoTWireless.CreateServiceProfile
import Amazonka.IoTWireless.CreateWirelessDevice
import Amazonka.IoTWireless.CreateWirelessGateway
import Amazonka.IoTWireless.CreateWirelessGatewayTask
import Amazonka.IoTWireless.CreateWirelessGatewayTaskDefinition
import Amazonka.IoTWireless.DeleteDestination
import Amazonka.IoTWireless.DeleteDeviceProfile
import Amazonka.IoTWireless.DeleteFuotaTask
import Amazonka.IoTWireless.DeleteMulticastGroup
import Amazonka.IoTWireless.DeleteNetworkAnalyzerConfiguration
import Amazonka.IoTWireless.DeleteQueuedMessages
import Amazonka.IoTWireless.DeleteServiceProfile
import Amazonka.IoTWireless.DeleteWirelessDevice
import Amazonka.IoTWireless.DeleteWirelessDeviceImportTask
import Amazonka.IoTWireless.DeleteWirelessGateway
import Amazonka.IoTWireless.DeleteWirelessGatewayTask
import Amazonka.IoTWireless.DeleteWirelessGatewayTaskDefinition
import Amazonka.IoTWireless.DeregisterWirelessDevice
import Amazonka.IoTWireless.DisassociateAwsAccountFromPartnerAccount
import Amazonka.IoTWireless.DisassociateMulticastGroupFromFuotaTask
import Amazonka.IoTWireless.DisassociateWirelessDeviceFromFuotaTask
import Amazonka.IoTWireless.DisassociateWirelessDeviceFromMulticastGroup
import Amazonka.IoTWireless.DisassociateWirelessDeviceFromThing
import Amazonka.IoTWireless.DisassociateWirelessGatewayFromCertificate
import Amazonka.IoTWireless.DisassociateWirelessGatewayFromThing
import Amazonka.IoTWireless.GetDestination
import Amazonka.IoTWireless.GetDeviceProfile
import Amazonka.IoTWireless.GetEventConfigurationByResourceTypes
import Amazonka.IoTWireless.GetFuotaTask
import Amazonka.IoTWireless.GetLogLevelsByResourceTypes
import Amazonka.IoTWireless.GetMulticastGroup
import Amazonka.IoTWireless.GetMulticastGroupSession
import Amazonka.IoTWireless.GetNetworkAnalyzerConfiguration
import Amazonka.IoTWireless.GetPartnerAccount
import Amazonka.IoTWireless.GetPositionEstimate
import Amazonka.IoTWireless.GetResourceEventConfiguration
import Amazonka.IoTWireless.GetResourceLogLevel
import Amazonka.IoTWireless.GetResourcePosition
import Amazonka.IoTWireless.GetServiceEndpoint
import Amazonka.IoTWireless.GetServiceProfile
import Amazonka.IoTWireless.GetWirelessDevice
import Amazonka.IoTWireless.GetWirelessDeviceImportTask
import Amazonka.IoTWireless.GetWirelessDeviceStatistics
import Amazonka.IoTWireless.GetWirelessGateway
import Amazonka.IoTWireless.GetWirelessGatewayCertificate
import Amazonka.IoTWireless.GetWirelessGatewayFirmwareInformation
import Amazonka.IoTWireless.GetWirelessGatewayStatistics
import Amazonka.IoTWireless.GetWirelessGatewayTask
import Amazonka.IoTWireless.GetWirelessGatewayTaskDefinition
import Amazonka.IoTWireless.ListDestinations
import Amazonka.IoTWireless.ListDeviceProfiles
import Amazonka.IoTWireless.ListDevicesForWirelessDeviceImportTask
import Amazonka.IoTWireless.ListEventConfigurations
import Amazonka.IoTWireless.ListFuotaTasks
import Amazonka.IoTWireless.ListMulticastGroups
import Amazonka.IoTWireless.ListMulticastGroupsByFuotaTask
import Amazonka.IoTWireless.ListNetworkAnalyzerConfigurations
import Amazonka.IoTWireless.ListPartnerAccounts
import Amazonka.IoTWireless.ListQueuedMessages
import Amazonka.IoTWireless.ListServiceProfiles
import Amazonka.IoTWireless.ListTagsForResource
import Amazonka.IoTWireless.ListWirelessDeviceImportTasks
import Amazonka.IoTWireless.ListWirelessDevices
import Amazonka.IoTWireless.ListWirelessGatewayTaskDefinitions
import Amazonka.IoTWireless.ListWirelessGateways
import Amazonka.IoTWireless.PutResourceLogLevel
import Amazonka.IoTWireless.ResetAllResourceLogLevels
import Amazonka.IoTWireless.ResetResourceLogLevel
import Amazonka.IoTWireless.SendDataToMulticastGroup
import Amazonka.IoTWireless.SendDataToWirelessDevice
import Amazonka.IoTWireless.StartBulkAssociateWirelessDeviceWithMulticastGroup
import Amazonka.IoTWireless.StartBulkDisassociateWirelessDeviceFromMulticastGroup
import Amazonka.IoTWireless.StartFuotaTask
import Amazonka.IoTWireless.StartMulticastGroupSession
import Amazonka.IoTWireless.StartSingleWirelessDeviceImportTask
import Amazonka.IoTWireless.StartWirelessDeviceImportTask
import Amazonka.IoTWireless.TagResource
import Amazonka.IoTWireless.TestWirelessDevice
import Amazonka.IoTWireless.Types.AbpV1_0_x
import Amazonka.IoTWireless.Types.AbpV1_1
import Amazonka.IoTWireless.Types.ApplicationConfig
import Amazonka.IoTWireless.Types.Beaconing
import Amazonka.IoTWireless.Types.CdmaLocalId
import Amazonka.IoTWireless.Types.CdmaNmrObj
import Amazonka.IoTWireless.Types.CdmaObj
import Amazonka.IoTWireless.Types.CellTowers
import Amazonka.IoTWireless.Types.CertificateList
import Amazonka.IoTWireless.Types.ConnectionStatusEventConfiguration
import Amazonka.IoTWireless.Types.ConnectionStatusResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.DakCertificateMetadata
import Amazonka.IoTWireless.Types.Destinations
import Amazonka.IoTWireless.Types.DeviceProfile
import Amazonka.IoTWireless.Types.DeviceRegistrationStateEventConfiguration
import Amazonka.IoTWireless.Types.DeviceRegistrationStateResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.DownlinkQueueMessage
import Amazonka.IoTWireless.Types.EventConfigurationItem
import Amazonka.IoTWireless.Types.EventNotificationItemConfigurations
import Amazonka.IoTWireless.Types.FPorts
import Amazonka.IoTWireless.Types.FuotaTask
import Amazonka.IoTWireless.Types.GatewayListItem
import Amazonka.IoTWireless.Types.GlobalIdentity
import Amazonka.IoTWireless.Types.Gnss
import Amazonka.IoTWireless.Types.GsmLocalId
import Amazonka.IoTWireless.Types.GsmNmrObj
import Amazonka.IoTWireless.Types.GsmObj
import Amazonka.IoTWireless.Types.ImportedSidewalkDevice
import Amazonka.IoTWireless.Types.ImportedWirelessDevice
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
import Amazonka.IoTWireless.Types.LteLocalId
import Amazonka.IoTWireless.Types.LteNmrObj
import Amazonka.IoTWireless.Types.LteObj
import Amazonka.IoTWireless.Types.MessageDeliveryStatusEventConfiguration
import Amazonka.IoTWireless.Types.MessageDeliveryStatusResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.MulticastGroup
import Amazonka.IoTWireless.Types.MulticastGroupByFuotaTask
import Amazonka.IoTWireless.Types.MulticastWirelessMetadata
import Amazonka.IoTWireless.Types.NetworkAnalyzerConfigurations
import Amazonka.IoTWireless.Types.OtaaV1_0_x
import Amazonka.IoTWireless.Types.OtaaV1_1
import Amazonka.IoTWireless.Types.ParticipatingGateways
import Amazonka.IoTWireless.Types.Positioning
import Amazonka.IoTWireless.Types.ProximityEventConfiguration
import Amazonka.IoTWireless.Types.ProximityResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.ServiceProfile
import Amazonka.IoTWireless.Types.SessionKeysAbpV1_0_x
import Amazonka.IoTWireless.Types.SessionKeysAbpV1_1
import Amazonka.IoTWireless.Types.SidewalkAccountInfo
import Amazonka.IoTWireless.Types.SidewalkAccountInfoWithFingerprint
import Amazonka.IoTWireless.Types.SidewalkCreateDeviceProfile
import Amazonka.IoTWireless.Types.SidewalkCreateWirelessDevice
import Amazonka.IoTWireless.Types.SidewalkDevice
import Amazonka.IoTWireless.Types.SidewalkDeviceMetadata
import Amazonka.IoTWireless.Types.SidewalkEventNotificationConfigurations
import Amazonka.IoTWireless.Types.SidewalkGetDeviceProfile
import Amazonka.IoTWireless.Types.SidewalkGetStartImportInfo
import Amazonka.IoTWireless.Types.SidewalkListDevice
import Amazonka.IoTWireless.Types.SidewalkResourceTypeEventConfiguration
import Amazonka.IoTWireless.Types.SidewalkSendDataToDevice
import Amazonka.IoTWireless.Types.SidewalkSingleStartImportInfo
import Amazonka.IoTWireless.Types.SidewalkStartImportInfo
import Amazonka.IoTWireless.Types.SidewalkUpdateAccount
import Amazonka.IoTWireless.Types.SidewalkUpdateImportInfo
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
import Amazonka.IoTWireless.Types.WirelessDeviceEventLogOption
import Amazonka.IoTWireless.Types.WirelessDeviceImportTask
import Amazonka.IoTWireless.Types.WirelessDeviceLogOption
import Amazonka.IoTWireless.Types.WirelessDeviceStatistics
import Amazonka.IoTWireless.Types.WirelessGatewayEventLogOption
import Amazonka.IoTWireless.Types.WirelessGatewayLogOption
import Amazonka.IoTWireless.Types.WirelessGatewayStatistics
import Amazonka.IoTWireless.Types.WirelessMetadata
import Amazonka.IoTWireless.UntagResource
import Amazonka.IoTWireless.UpdateDestination
import Amazonka.IoTWireless.UpdateEventConfigurationByResourceTypes
import Amazonka.IoTWireless.UpdateFuotaTask
import Amazonka.IoTWireless.UpdateLogLevelsByResourceTypes
import Amazonka.IoTWireless.UpdateMulticastGroup
import Amazonka.IoTWireless.UpdateNetworkAnalyzerConfiguration
import Amazonka.IoTWireless.UpdatePartnerAccount
import Amazonka.IoTWireless.UpdateResourceEventConfiguration
import Amazonka.IoTWireless.UpdateResourcePosition
import Amazonka.IoTWireless.UpdateWirelessDevice
import Amazonka.IoTWireless.UpdateWirelessDeviceImportTask
import Amazonka.IoTWireless.UpdateWirelessGateway
