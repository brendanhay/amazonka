{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTWireless
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-22@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Wireless provides bi-directional communication between
-- internet-connected wireless devices and the AWS Cloud. To onboard both
-- LoRaWAN and Sidewalk devices to AWS IoT, use the IoT Wireless API. These
-- wireless devices use the Low Power Wide Area Networking (LPWAN)
-- communication protocol to communicate with AWS IoT.
--
-- Using the API, you can perform create, read, update, and delete
-- operations for your wireless devices, gateways, destinations, and
-- profiles. After onboarding your devices, you can use the API operations
-- to set log levels and monitor your devices with CloudWatch.
--
-- You can also use the API operations to create multicast groups and
-- schedule a multicast session for sending a downlink message to devices
-- in the group. By using Firmware Updates Over-The-Air (FUOTA) API
-- operations, you can create a FUOTA task and schedule a session to update
-- the firmware of individual devices or an entire group of devices in a
-- multicast group.
module Amazonka.IoTWireless
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateAwsAccountWithPartnerAccount
    AssociateAwsAccountWithPartnerAccount (AssociateAwsAccountWithPartnerAccount'),
    newAssociateAwsAccountWithPartnerAccount,
    AssociateAwsAccountWithPartnerAccountResponse (AssociateAwsAccountWithPartnerAccountResponse'),
    newAssociateAwsAccountWithPartnerAccountResponse,

    -- ** AssociateMulticastGroupWithFuotaTask
    AssociateMulticastGroupWithFuotaTask (AssociateMulticastGroupWithFuotaTask'),
    newAssociateMulticastGroupWithFuotaTask,
    AssociateMulticastGroupWithFuotaTaskResponse (AssociateMulticastGroupWithFuotaTaskResponse'),
    newAssociateMulticastGroupWithFuotaTaskResponse,

    -- ** AssociateWirelessDeviceWithFuotaTask
    AssociateWirelessDeviceWithFuotaTask (AssociateWirelessDeviceWithFuotaTask'),
    newAssociateWirelessDeviceWithFuotaTask,
    AssociateWirelessDeviceWithFuotaTaskResponse (AssociateWirelessDeviceWithFuotaTaskResponse'),
    newAssociateWirelessDeviceWithFuotaTaskResponse,

    -- ** AssociateWirelessDeviceWithMulticastGroup
    AssociateWirelessDeviceWithMulticastGroup (AssociateWirelessDeviceWithMulticastGroup'),
    newAssociateWirelessDeviceWithMulticastGroup,
    AssociateWirelessDeviceWithMulticastGroupResponse (AssociateWirelessDeviceWithMulticastGroupResponse'),
    newAssociateWirelessDeviceWithMulticastGroupResponse,

    -- ** AssociateWirelessDeviceWithThing
    AssociateWirelessDeviceWithThing (AssociateWirelessDeviceWithThing'),
    newAssociateWirelessDeviceWithThing,
    AssociateWirelessDeviceWithThingResponse (AssociateWirelessDeviceWithThingResponse'),
    newAssociateWirelessDeviceWithThingResponse,

    -- ** AssociateWirelessGatewayWithCertificate
    AssociateWirelessGatewayWithCertificate (AssociateWirelessGatewayWithCertificate'),
    newAssociateWirelessGatewayWithCertificate,
    AssociateWirelessGatewayWithCertificateResponse (AssociateWirelessGatewayWithCertificateResponse'),
    newAssociateWirelessGatewayWithCertificateResponse,

    -- ** AssociateWirelessGatewayWithThing
    AssociateWirelessGatewayWithThing (AssociateWirelessGatewayWithThing'),
    newAssociateWirelessGatewayWithThing,
    AssociateWirelessGatewayWithThingResponse (AssociateWirelessGatewayWithThingResponse'),
    newAssociateWirelessGatewayWithThingResponse,

    -- ** CancelMulticastGroupSession
    CancelMulticastGroupSession (CancelMulticastGroupSession'),
    newCancelMulticastGroupSession,
    CancelMulticastGroupSessionResponse (CancelMulticastGroupSessionResponse'),
    newCancelMulticastGroupSessionResponse,

    -- ** CreateDestination
    CreateDestination (CreateDestination'),
    newCreateDestination,
    CreateDestinationResponse (CreateDestinationResponse'),
    newCreateDestinationResponse,

    -- ** CreateDeviceProfile
    CreateDeviceProfile (CreateDeviceProfile'),
    newCreateDeviceProfile,
    CreateDeviceProfileResponse (CreateDeviceProfileResponse'),
    newCreateDeviceProfileResponse,

    -- ** CreateFuotaTask
    CreateFuotaTask (CreateFuotaTask'),
    newCreateFuotaTask,
    CreateFuotaTaskResponse (CreateFuotaTaskResponse'),
    newCreateFuotaTaskResponse,

    -- ** CreateMulticastGroup
    CreateMulticastGroup (CreateMulticastGroup'),
    newCreateMulticastGroup,
    CreateMulticastGroupResponse (CreateMulticastGroupResponse'),
    newCreateMulticastGroupResponse,

    -- ** CreateNetworkAnalyzerConfiguration
    CreateNetworkAnalyzerConfiguration (CreateNetworkAnalyzerConfiguration'),
    newCreateNetworkAnalyzerConfiguration,
    CreateNetworkAnalyzerConfigurationResponse (CreateNetworkAnalyzerConfigurationResponse'),
    newCreateNetworkAnalyzerConfigurationResponse,

    -- ** CreateServiceProfile
    CreateServiceProfile (CreateServiceProfile'),
    newCreateServiceProfile,
    CreateServiceProfileResponse (CreateServiceProfileResponse'),
    newCreateServiceProfileResponse,

    -- ** CreateWirelessDevice
    CreateWirelessDevice (CreateWirelessDevice'),
    newCreateWirelessDevice,
    CreateWirelessDeviceResponse (CreateWirelessDeviceResponse'),
    newCreateWirelessDeviceResponse,

    -- ** CreateWirelessGateway
    CreateWirelessGateway (CreateWirelessGateway'),
    newCreateWirelessGateway,
    CreateWirelessGatewayResponse (CreateWirelessGatewayResponse'),
    newCreateWirelessGatewayResponse,

    -- ** CreateWirelessGatewayTask
    CreateWirelessGatewayTask (CreateWirelessGatewayTask'),
    newCreateWirelessGatewayTask,
    CreateWirelessGatewayTaskResponse (CreateWirelessGatewayTaskResponse'),
    newCreateWirelessGatewayTaskResponse,

    -- ** CreateWirelessGatewayTaskDefinition
    CreateWirelessGatewayTaskDefinition (CreateWirelessGatewayTaskDefinition'),
    newCreateWirelessGatewayTaskDefinition,
    CreateWirelessGatewayTaskDefinitionResponse (CreateWirelessGatewayTaskDefinitionResponse'),
    newCreateWirelessGatewayTaskDefinitionResponse,

    -- ** DeleteDestination
    DeleteDestination (DeleteDestination'),
    newDeleteDestination,
    DeleteDestinationResponse (DeleteDestinationResponse'),
    newDeleteDestinationResponse,

    -- ** DeleteDeviceProfile
    DeleteDeviceProfile (DeleteDeviceProfile'),
    newDeleteDeviceProfile,
    DeleteDeviceProfileResponse (DeleteDeviceProfileResponse'),
    newDeleteDeviceProfileResponse,

    -- ** DeleteFuotaTask
    DeleteFuotaTask (DeleteFuotaTask'),
    newDeleteFuotaTask,
    DeleteFuotaTaskResponse (DeleteFuotaTaskResponse'),
    newDeleteFuotaTaskResponse,

    -- ** DeleteMulticastGroup
    DeleteMulticastGroup (DeleteMulticastGroup'),
    newDeleteMulticastGroup,
    DeleteMulticastGroupResponse (DeleteMulticastGroupResponse'),
    newDeleteMulticastGroupResponse,

    -- ** DeleteNetworkAnalyzerConfiguration
    DeleteNetworkAnalyzerConfiguration (DeleteNetworkAnalyzerConfiguration'),
    newDeleteNetworkAnalyzerConfiguration,
    DeleteNetworkAnalyzerConfigurationResponse (DeleteNetworkAnalyzerConfigurationResponse'),
    newDeleteNetworkAnalyzerConfigurationResponse,

    -- ** DeleteQueuedMessages
    DeleteQueuedMessages (DeleteQueuedMessages'),
    newDeleteQueuedMessages,
    DeleteQueuedMessagesResponse (DeleteQueuedMessagesResponse'),
    newDeleteQueuedMessagesResponse,

    -- ** DeleteServiceProfile
    DeleteServiceProfile (DeleteServiceProfile'),
    newDeleteServiceProfile,
    DeleteServiceProfileResponse (DeleteServiceProfileResponse'),
    newDeleteServiceProfileResponse,

    -- ** DeleteWirelessDevice
    DeleteWirelessDevice (DeleteWirelessDevice'),
    newDeleteWirelessDevice,
    DeleteWirelessDeviceResponse (DeleteWirelessDeviceResponse'),
    newDeleteWirelessDeviceResponse,

    -- ** DeleteWirelessGateway
    DeleteWirelessGateway (DeleteWirelessGateway'),
    newDeleteWirelessGateway,
    DeleteWirelessGatewayResponse (DeleteWirelessGatewayResponse'),
    newDeleteWirelessGatewayResponse,

    -- ** DeleteWirelessGatewayTask
    DeleteWirelessGatewayTask (DeleteWirelessGatewayTask'),
    newDeleteWirelessGatewayTask,
    DeleteWirelessGatewayTaskResponse (DeleteWirelessGatewayTaskResponse'),
    newDeleteWirelessGatewayTaskResponse,

    -- ** DeleteWirelessGatewayTaskDefinition
    DeleteWirelessGatewayTaskDefinition (DeleteWirelessGatewayTaskDefinition'),
    newDeleteWirelessGatewayTaskDefinition,
    DeleteWirelessGatewayTaskDefinitionResponse (DeleteWirelessGatewayTaskDefinitionResponse'),
    newDeleteWirelessGatewayTaskDefinitionResponse,

    -- ** DisassociateAwsAccountFromPartnerAccount
    DisassociateAwsAccountFromPartnerAccount (DisassociateAwsAccountFromPartnerAccount'),
    newDisassociateAwsAccountFromPartnerAccount,
    DisassociateAwsAccountFromPartnerAccountResponse (DisassociateAwsAccountFromPartnerAccountResponse'),
    newDisassociateAwsAccountFromPartnerAccountResponse,

    -- ** DisassociateMulticastGroupFromFuotaTask
    DisassociateMulticastGroupFromFuotaTask (DisassociateMulticastGroupFromFuotaTask'),
    newDisassociateMulticastGroupFromFuotaTask,
    DisassociateMulticastGroupFromFuotaTaskResponse (DisassociateMulticastGroupFromFuotaTaskResponse'),
    newDisassociateMulticastGroupFromFuotaTaskResponse,

    -- ** DisassociateWirelessDeviceFromFuotaTask
    DisassociateWirelessDeviceFromFuotaTask (DisassociateWirelessDeviceFromFuotaTask'),
    newDisassociateWirelessDeviceFromFuotaTask,
    DisassociateWirelessDeviceFromFuotaTaskResponse (DisassociateWirelessDeviceFromFuotaTaskResponse'),
    newDisassociateWirelessDeviceFromFuotaTaskResponse,

    -- ** DisassociateWirelessDeviceFromMulticastGroup
    DisassociateWirelessDeviceFromMulticastGroup (DisassociateWirelessDeviceFromMulticastGroup'),
    newDisassociateWirelessDeviceFromMulticastGroup,
    DisassociateWirelessDeviceFromMulticastGroupResponse (DisassociateWirelessDeviceFromMulticastGroupResponse'),
    newDisassociateWirelessDeviceFromMulticastGroupResponse,

    -- ** DisassociateWirelessDeviceFromThing
    DisassociateWirelessDeviceFromThing (DisassociateWirelessDeviceFromThing'),
    newDisassociateWirelessDeviceFromThing,
    DisassociateWirelessDeviceFromThingResponse (DisassociateWirelessDeviceFromThingResponse'),
    newDisassociateWirelessDeviceFromThingResponse,

    -- ** DisassociateWirelessGatewayFromCertificate
    DisassociateWirelessGatewayFromCertificate (DisassociateWirelessGatewayFromCertificate'),
    newDisassociateWirelessGatewayFromCertificate,
    DisassociateWirelessGatewayFromCertificateResponse (DisassociateWirelessGatewayFromCertificateResponse'),
    newDisassociateWirelessGatewayFromCertificateResponse,

    -- ** DisassociateWirelessGatewayFromThing
    DisassociateWirelessGatewayFromThing (DisassociateWirelessGatewayFromThing'),
    newDisassociateWirelessGatewayFromThing,
    DisassociateWirelessGatewayFromThingResponse (DisassociateWirelessGatewayFromThingResponse'),
    newDisassociateWirelessGatewayFromThingResponse,

    -- ** GetDestination
    GetDestination (GetDestination'),
    newGetDestination,
    GetDestinationResponse (GetDestinationResponse'),
    newGetDestinationResponse,

    -- ** GetDeviceProfile
    GetDeviceProfile (GetDeviceProfile'),
    newGetDeviceProfile,
    GetDeviceProfileResponse (GetDeviceProfileResponse'),
    newGetDeviceProfileResponse,

    -- ** GetEventConfigurationByResourceTypes
    GetEventConfigurationByResourceTypes (GetEventConfigurationByResourceTypes'),
    newGetEventConfigurationByResourceTypes,
    GetEventConfigurationByResourceTypesResponse (GetEventConfigurationByResourceTypesResponse'),
    newGetEventConfigurationByResourceTypesResponse,

    -- ** GetFuotaTask
    GetFuotaTask (GetFuotaTask'),
    newGetFuotaTask,
    GetFuotaTaskResponse (GetFuotaTaskResponse'),
    newGetFuotaTaskResponse,

    -- ** GetLogLevelsByResourceTypes
    GetLogLevelsByResourceTypes (GetLogLevelsByResourceTypes'),
    newGetLogLevelsByResourceTypes,
    GetLogLevelsByResourceTypesResponse (GetLogLevelsByResourceTypesResponse'),
    newGetLogLevelsByResourceTypesResponse,

    -- ** GetMulticastGroup
    GetMulticastGroup (GetMulticastGroup'),
    newGetMulticastGroup,
    GetMulticastGroupResponse (GetMulticastGroupResponse'),
    newGetMulticastGroupResponse,

    -- ** GetMulticastGroupSession
    GetMulticastGroupSession (GetMulticastGroupSession'),
    newGetMulticastGroupSession,
    GetMulticastGroupSessionResponse (GetMulticastGroupSessionResponse'),
    newGetMulticastGroupSessionResponse,

    -- ** GetNetworkAnalyzerConfiguration
    GetNetworkAnalyzerConfiguration (GetNetworkAnalyzerConfiguration'),
    newGetNetworkAnalyzerConfiguration,
    GetNetworkAnalyzerConfigurationResponse (GetNetworkAnalyzerConfigurationResponse'),
    newGetNetworkAnalyzerConfigurationResponse,

    -- ** GetPartnerAccount
    GetPartnerAccount (GetPartnerAccount'),
    newGetPartnerAccount,
    GetPartnerAccountResponse (GetPartnerAccountResponse'),
    newGetPartnerAccountResponse,

    -- ** GetPositionEstimate
    GetPositionEstimate (GetPositionEstimate'),
    newGetPositionEstimate,
    GetPositionEstimateResponse (GetPositionEstimateResponse'),
    newGetPositionEstimateResponse,

    -- ** GetResourceEventConfiguration
    GetResourceEventConfiguration (GetResourceEventConfiguration'),
    newGetResourceEventConfiguration,
    GetResourceEventConfigurationResponse (GetResourceEventConfigurationResponse'),
    newGetResourceEventConfigurationResponse,

    -- ** GetResourceLogLevel
    GetResourceLogLevel (GetResourceLogLevel'),
    newGetResourceLogLevel,
    GetResourceLogLevelResponse (GetResourceLogLevelResponse'),
    newGetResourceLogLevelResponse,

    -- ** GetResourcePosition
    GetResourcePosition (GetResourcePosition'),
    newGetResourcePosition,
    GetResourcePositionResponse (GetResourcePositionResponse'),
    newGetResourcePositionResponse,

    -- ** GetServiceEndpoint
    GetServiceEndpoint (GetServiceEndpoint'),
    newGetServiceEndpoint,
    GetServiceEndpointResponse (GetServiceEndpointResponse'),
    newGetServiceEndpointResponse,

    -- ** GetServiceProfile
    GetServiceProfile (GetServiceProfile'),
    newGetServiceProfile,
    GetServiceProfileResponse (GetServiceProfileResponse'),
    newGetServiceProfileResponse,

    -- ** GetWirelessDevice
    GetWirelessDevice (GetWirelessDevice'),
    newGetWirelessDevice,
    GetWirelessDeviceResponse (GetWirelessDeviceResponse'),
    newGetWirelessDeviceResponse,

    -- ** GetWirelessDeviceStatistics
    GetWirelessDeviceStatistics (GetWirelessDeviceStatistics'),
    newGetWirelessDeviceStatistics,
    GetWirelessDeviceStatisticsResponse (GetWirelessDeviceStatisticsResponse'),
    newGetWirelessDeviceStatisticsResponse,

    -- ** GetWirelessGateway
    GetWirelessGateway (GetWirelessGateway'),
    newGetWirelessGateway,
    GetWirelessGatewayResponse (GetWirelessGatewayResponse'),
    newGetWirelessGatewayResponse,

    -- ** GetWirelessGatewayCertificate
    GetWirelessGatewayCertificate (GetWirelessGatewayCertificate'),
    newGetWirelessGatewayCertificate,
    GetWirelessGatewayCertificateResponse (GetWirelessGatewayCertificateResponse'),
    newGetWirelessGatewayCertificateResponse,

    -- ** GetWirelessGatewayFirmwareInformation
    GetWirelessGatewayFirmwareInformation (GetWirelessGatewayFirmwareInformation'),
    newGetWirelessGatewayFirmwareInformation,
    GetWirelessGatewayFirmwareInformationResponse (GetWirelessGatewayFirmwareInformationResponse'),
    newGetWirelessGatewayFirmwareInformationResponse,

    -- ** GetWirelessGatewayStatistics
    GetWirelessGatewayStatistics (GetWirelessGatewayStatistics'),
    newGetWirelessGatewayStatistics,
    GetWirelessGatewayStatisticsResponse (GetWirelessGatewayStatisticsResponse'),
    newGetWirelessGatewayStatisticsResponse,

    -- ** GetWirelessGatewayTask
    GetWirelessGatewayTask (GetWirelessGatewayTask'),
    newGetWirelessGatewayTask,
    GetWirelessGatewayTaskResponse (GetWirelessGatewayTaskResponse'),
    newGetWirelessGatewayTaskResponse,

    -- ** GetWirelessGatewayTaskDefinition
    GetWirelessGatewayTaskDefinition (GetWirelessGatewayTaskDefinition'),
    newGetWirelessGatewayTaskDefinition,
    GetWirelessGatewayTaskDefinitionResponse (GetWirelessGatewayTaskDefinitionResponse'),
    newGetWirelessGatewayTaskDefinitionResponse,

    -- ** ListDestinations
    ListDestinations (ListDestinations'),
    newListDestinations,
    ListDestinationsResponse (ListDestinationsResponse'),
    newListDestinationsResponse,

    -- ** ListDeviceProfiles
    ListDeviceProfiles (ListDeviceProfiles'),
    newListDeviceProfiles,
    ListDeviceProfilesResponse (ListDeviceProfilesResponse'),
    newListDeviceProfilesResponse,

    -- ** ListEventConfigurations
    ListEventConfigurations (ListEventConfigurations'),
    newListEventConfigurations,
    ListEventConfigurationsResponse (ListEventConfigurationsResponse'),
    newListEventConfigurationsResponse,

    -- ** ListFuotaTasks
    ListFuotaTasks (ListFuotaTasks'),
    newListFuotaTasks,
    ListFuotaTasksResponse (ListFuotaTasksResponse'),
    newListFuotaTasksResponse,

    -- ** ListMulticastGroups
    ListMulticastGroups (ListMulticastGroups'),
    newListMulticastGroups,
    ListMulticastGroupsResponse (ListMulticastGroupsResponse'),
    newListMulticastGroupsResponse,

    -- ** ListMulticastGroupsByFuotaTask
    ListMulticastGroupsByFuotaTask (ListMulticastGroupsByFuotaTask'),
    newListMulticastGroupsByFuotaTask,
    ListMulticastGroupsByFuotaTaskResponse (ListMulticastGroupsByFuotaTaskResponse'),
    newListMulticastGroupsByFuotaTaskResponse,

    -- ** ListNetworkAnalyzerConfigurations
    ListNetworkAnalyzerConfigurations (ListNetworkAnalyzerConfigurations'),
    newListNetworkAnalyzerConfigurations,
    ListNetworkAnalyzerConfigurationsResponse (ListNetworkAnalyzerConfigurationsResponse'),
    newListNetworkAnalyzerConfigurationsResponse,

    -- ** ListPartnerAccounts
    ListPartnerAccounts (ListPartnerAccounts'),
    newListPartnerAccounts,
    ListPartnerAccountsResponse (ListPartnerAccountsResponse'),
    newListPartnerAccountsResponse,

    -- ** ListQueuedMessages
    ListQueuedMessages (ListQueuedMessages'),
    newListQueuedMessages,
    ListQueuedMessagesResponse (ListQueuedMessagesResponse'),
    newListQueuedMessagesResponse,

    -- ** ListServiceProfiles
    ListServiceProfiles (ListServiceProfiles'),
    newListServiceProfiles,
    ListServiceProfilesResponse (ListServiceProfilesResponse'),
    newListServiceProfilesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWirelessDevices
    ListWirelessDevices (ListWirelessDevices'),
    newListWirelessDevices,
    ListWirelessDevicesResponse (ListWirelessDevicesResponse'),
    newListWirelessDevicesResponse,

    -- ** ListWirelessGatewayTaskDefinitions
    ListWirelessGatewayTaskDefinitions (ListWirelessGatewayTaskDefinitions'),
    newListWirelessGatewayTaskDefinitions,
    ListWirelessGatewayTaskDefinitionsResponse (ListWirelessGatewayTaskDefinitionsResponse'),
    newListWirelessGatewayTaskDefinitionsResponse,

    -- ** ListWirelessGateways
    ListWirelessGateways (ListWirelessGateways'),
    newListWirelessGateways,
    ListWirelessGatewaysResponse (ListWirelessGatewaysResponse'),
    newListWirelessGatewaysResponse,

    -- ** PutResourceLogLevel
    PutResourceLogLevel (PutResourceLogLevel'),
    newPutResourceLogLevel,
    PutResourceLogLevelResponse (PutResourceLogLevelResponse'),
    newPutResourceLogLevelResponse,

    -- ** ResetAllResourceLogLevels
    ResetAllResourceLogLevels (ResetAllResourceLogLevels'),
    newResetAllResourceLogLevels,
    ResetAllResourceLogLevelsResponse (ResetAllResourceLogLevelsResponse'),
    newResetAllResourceLogLevelsResponse,

    -- ** ResetResourceLogLevel
    ResetResourceLogLevel (ResetResourceLogLevel'),
    newResetResourceLogLevel,
    ResetResourceLogLevelResponse (ResetResourceLogLevelResponse'),
    newResetResourceLogLevelResponse,

    -- ** SendDataToMulticastGroup
    SendDataToMulticastGroup (SendDataToMulticastGroup'),
    newSendDataToMulticastGroup,
    SendDataToMulticastGroupResponse (SendDataToMulticastGroupResponse'),
    newSendDataToMulticastGroupResponse,

    -- ** SendDataToWirelessDevice
    SendDataToWirelessDevice (SendDataToWirelessDevice'),
    newSendDataToWirelessDevice,
    SendDataToWirelessDeviceResponse (SendDataToWirelessDeviceResponse'),
    newSendDataToWirelessDeviceResponse,

    -- ** StartBulkAssociateWirelessDeviceWithMulticastGroup
    StartBulkAssociateWirelessDeviceWithMulticastGroup (StartBulkAssociateWirelessDeviceWithMulticastGroup'),
    newStartBulkAssociateWirelessDeviceWithMulticastGroup,
    StartBulkAssociateWirelessDeviceWithMulticastGroupResponse (StartBulkAssociateWirelessDeviceWithMulticastGroupResponse'),
    newStartBulkAssociateWirelessDeviceWithMulticastGroupResponse,

    -- ** StartBulkDisassociateWirelessDeviceFromMulticastGroup
    StartBulkDisassociateWirelessDeviceFromMulticastGroup (StartBulkDisassociateWirelessDeviceFromMulticastGroup'),
    newStartBulkDisassociateWirelessDeviceFromMulticastGroup,
    StartBulkDisassociateWirelessDeviceFromMulticastGroupResponse (StartBulkDisassociateWirelessDeviceFromMulticastGroupResponse'),
    newStartBulkDisassociateWirelessDeviceFromMulticastGroupResponse,

    -- ** StartFuotaTask
    StartFuotaTask (StartFuotaTask'),
    newStartFuotaTask,
    StartFuotaTaskResponse (StartFuotaTaskResponse'),
    newStartFuotaTaskResponse,

    -- ** StartMulticastGroupSession
    StartMulticastGroupSession (StartMulticastGroupSession'),
    newStartMulticastGroupSession,
    StartMulticastGroupSessionResponse (StartMulticastGroupSessionResponse'),
    newStartMulticastGroupSessionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestWirelessDevice
    TestWirelessDevice (TestWirelessDevice'),
    newTestWirelessDevice,
    TestWirelessDeviceResponse (TestWirelessDeviceResponse'),
    newTestWirelessDeviceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateDestination
    UpdateDestination (UpdateDestination'),
    newUpdateDestination,
    UpdateDestinationResponse (UpdateDestinationResponse'),
    newUpdateDestinationResponse,

    -- ** UpdateEventConfigurationByResourceTypes
    UpdateEventConfigurationByResourceTypes (UpdateEventConfigurationByResourceTypes'),
    newUpdateEventConfigurationByResourceTypes,
    UpdateEventConfigurationByResourceTypesResponse (UpdateEventConfigurationByResourceTypesResponse'),
    newUpdateEventConfigurationByResourceTypesResponse,

    -- ** UpdateFuotaTask
    UpdateFuotaTask (UpdateFuotaTask'),
    newUpdateFuotaTask,
    UpdateFuotaTaskResponse (UpdateFuotaTaskResponse'),
    newUpdateFuotaTaskResponse,

    -- ** UpdateLogLevelsByResourceTypes
    UpdateLogLevelsByResourceTypes (UpdateLogLevelsByResourceTypes'),
    newUpdateLogLevelsByResourceTypes,
    UpdateLogLevelsByResourceTypesResponse (UpdateLogLevelsByResourceTypesResponse'),
    newUpdateLogLevelsByResourceTypesResponse,

    -- ** UpdateMulticastGroup
    UpdateMulticastGroup (UpdateMulticastGroup'),
    newUpdateMulticastGroup,
    UpdateMulticastGroupResponse (UpdateMulticastGroupResponse'),
    newUpdateMulticastGroupResponse,

    -- ** UpdateNetworkAnalyzerConfiguration
    UpdateNetworkAnalyzerConfiguration (UpdateNetworkAnalyzerConfiguration'),
    newUpdateNetworkAnalyzerConfiguration,
    UpdateNetworkAnalyzerConfigurationResponse (UpdateNetworkAnalyzerConfigurationResponse'),
    newUpdateNetworkAnalyzerConfigurationResponse,

    -- ** UpdatePartnerAccount
    UpdatePartnerAccount (UpdatePartnerAccount'),
    newUpdatePartnerAccount,
    UpdatePartnerAccountResponse (UpdatePartnerAccountResponse'),
    newUpdatePartnerAccountResponse,

    -- ** UpdateResourceEventConfiguration
    UpdateResourceEventConfiguration (UpdateResourceEventConfiguration'),
    newUpdateResourceEventConfiguration,
    UpdateResourceEventConfigurationResponse (UpdateResourceEventConfigurationResponse'),
    newUpdateResourceEventConfigurationResponse,

    -- ** UpdateResourcePosition
    UpdateResourcePosition (UpdateResourcePosition'),
    newUpdateResourcePosition,
    UpdateResourcePositionResponse (UpdateResourcePositionResponse'),
    newUpdateResourcePositionResponse,

    -- ** UpdateWirelessDevice
    UpdateWirelessDevice (UpdateWirelessDevice'),
    newUpdateWirelessDevice,
    UpdateWirelessDeviceResponse (UpdateWirelessDeviceResponse'),
    newUpdateWirelessDeviceResponse,

    -- ** UpdateWirelessGateway
    UpdateWirelessGateway (UpdateWirelessGateway'),
    newUpdateWirelessGateway,
    UpdateWirelessGatewayResponse (UpdateWirelessGatewayResponse'),
    newUpdateWirelessGatewayResponse,

    -- * Types

    -- ** ApplicationConfigType
    ApplicationConfigType (..),

    -- ** BatteryLevel
    BatteryLevel (..),

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** DeviceState
    DeviceState (..),

    -- ** DlClass
    DlClass (..),

    -- ** DownlinkMode
    DownlinkMode (..),

    -- ** Event
    Event (..),

    -- ** EventNotificationPartnerType
    EventNotificationPartnerType (..),

    -- ** EventNotificationResourceType
    EventNotificationResourceType (..),

    -- ** EventNotificationTopicStatus
    EventNotificationTopicStatus (..),

    -- ** ExpressionType
    ExpressionType (..),

    -- ** FuotaDeviceStatus
    FuotaDeviceStatus (..),

    -- ** FuotaTaskStatus
    FuotaTaskStatus (..),

    -- ** IdentifierType
    IdentifierType (..),

    -- ** LogLevel
    LogLevel (..),

    -- ** MessageType
    MessageType (..),

    -- ** PartnerType
    PartnerType (..),

    -- ** PositionResourceType
    PositionResourceType (..),

    -- ** PositioningConfigStatus
    PositioningConfigStatus (..),

    -- ** SigningAlg
    SigningAlg (..),

    -- ** SupportedRfRegion
    SupportedRfRegion (..),

    -- ** WirelessDeviceEvent
    WirelessDeviceEvent (..),

    -- ** WirelessDeviceFrameInfo
    WirelessDeviceFrameInfo (..),

    -- ** WirelessDeviceIdType
    WirelessDeviceIdType (..),

    -- ** WirelessDeviceType
    WirelessDeviceType (..),

    -- ** WirelessGatewayEvent
    WirelessGatewayEvent (..),

    -- ** WirelessGatewayIdType
    WirelessGatewayIdType (..),

    -- ** WirelessGatewayServiceType
    WirelessGatewayServiceType (..),

    -- ** WirelessGatewayTaskDefinitionType
    WirelessGatewayTaskDefinitionType (..),

    -- ** WirelessGatewayTaskStatus
    WirelessGatewayTaskStatus (..),

    -- ** WirelessGatewayType
    WirelessGatewayType (..),

    -- ** AbpV1_0_x
    AbpV1_0_x (AbpV1_0_x'),
    newAbpV1_0_x,

    -- ** AbpV1_1
    AbpV1_1 (AbpV1_1'),
    newAbpV1_1,

    -- ** ApplicationConfig
    ApplicationConfig (ApplicationConfig'),
    newApplicationConfig,

    -- ** Beaconing
    Beaconing (Beaconing'),
    newBeaconing,

    -- ** CdmaLocalId
    CdmaLocalId (CdmaLocalId'),
    newCdmaLocalId,

    -- ** CdmaNmrObj
    CdmaNmrObj (CdmaNmrObj'),
    newCdmaNmrObj,

    -- ** CdmaObj
    CdmaObj (CdmaObj'),
    newCdmaObj,

    -- ** CellTowers
    CellTowers (CellTowers'),
    newCellTowers,

    -- ** CertificateList
    CertificateList (CertificateList'),
    newCertificateList,

    -- ** ConnectionStatusEventConfiguration
    ConnectionStatusEventConfiguration (ConnectionStatusEventConfiguration'),
    newConnectionStatusEventConfiguration,

    -- ** ConnectionStatusResourceTypeEventConfiguration
    ConnectionStatusResourceTypeEventConfiguration (ConnectionStatusResourceTypeEventConfiguration'),
    newConnectionStatusResourceTypeEventConfiguration,

    -- ** Destinations
    Destinations (Destinations'),
    newDestinations,

    -- ** DeviceProfile
    DeviceProfile (DeviceProfile'),
    newDeviceProfile,

    -- ** DeviceRegistrationStateEventConfiguration
    DeviceRegistrationStateEventConfiguration (DeviceRegistrationStateEventConfiguration'),
    newDeviceRegistrationStateEventConfiguration,

    -- ** DeviceRegistrationStateResourceTypeEventConfiguration
    DeviceRegistrationStateResourceTypeEventConfiguration (DeviceRegistrationStateResourceTypeEventConfiguration'),
    newDeviceRegistrationStateResourceTypeEventConfiguration,

    -- ** DownlinkQueueMessage
    DownlinkQueueMessage (DownlinkQueueMessage'),
    newDownlinkQueueMessage,

    -- ** EventConfigurationItem
    EventConfigurationItem (EventConfigurationItem'),
    newEventConfigurationItem,

    -- ** EventNotificationItemConfigurations
    EventNotificationItemConfigurations (EventNotificationItemConfigurations'),
    newEventNotificationItemConfigurations,

    -- ** FPorts
    FPorts (FPorts'),
    newFPorts,

    -- ** FuotaTask
    FuotaTask (FuotaTask'),
    newFuotaTask,

    -- ** GatewayListItem
    GatewayListItem (GatewayListItem'),
    newGatewayListItem,

    -- ** GlobalIdentity
    GlobalIdentity (GlobalIdentity'),
    newGlobalIdentity,

    -- ** Gnss
    Gnss (Gnss'),
    newGnss,

    -- ** GsmLocalId
    GsmLocalId (GsmLocalId'),
    newGsmLocalId,

    -- ** GsmNmrObj
    GsmNmrObj (GsmNmrObj'),
    newGsmNmrObj,

    -- ** GsmObj
    GsmObj (GsmObj'),
    newGsmObj,

    -- ** Ip
    Ip (Ip'),
    newIp,

    -- ** JoinEventConfiguration
    JoinEventConfiguration (JoinEventConfiguration'),
    newJoinEventConfiguration,

    -- ** JoinResourceTypeEventConfiguration
    JoinResourceTypeEventConfiguration (JoinResourceTypeEventConfiguration'),
    newJoinResourceTypeEventConfiguration,

    -- ** LoRaWANConnectionStatusEventNotificationConfigurations
    LoRaWANConnectionStatusEventNotificationConfigurations (LoRaWANConnectionStatusEventNotificationConfigurations'),
    newLoRaWANConnectionStatusEventNotificationConfigurations,

    -- ** LoRaWANConnectionStatusResourceTypeEventConfiguration
    LoRaWANConnectionStatusResourceTypeEventConfiguration (LoRaWANConnectionStatusResourceTypeEventConfiguration'),
    newLoRaWANConnectionStatusResourceTypeEventConfiguration,

    -- ** LoRaWANDevice
    LoRaWANDevice (LoRaWANDevice'),
    newLoRaWANDevice,

    -- ** LoRaWANDeviceMetadata
    LoRaWANDeviceMetadata (LoRaWANDeviceMetadata'),
    newLoRaWANDeviceMetadata,

    -- ** LoRaWANDeviceProfile
    LoRaWANDeviceProfile (LoRaWANDeviceProfile'),
    newLoRaWANDeviceProfile,

    -- ** LoRaWANFuotaTask
    LoRaWANFuotaTask (LoRaWANFuotaTask'),
    newLoRaWANFuotaTask,

    -- ** LoRaWANFuotaTaskGetInfo
    LoRaWANFuotaTaskGetInfo (LoRaWANFuotaTaskGetInfo'),
    newLoRaWANFuotaTaskGetInfo,

    -- ** LoRaWANGateway
    LoRaWANGateway (LoRaWANGateway'),
    newLoRaWANGateway,

    -- ** LoRaWANGatewayCurrentVersion
    LoRaWANGatewayCurrentVersion (LoRaWANGatewayCurrentVersion'),
    newLoRaWANGatewayCurrentVersion,

    -- ** LoRaWANGatewayMetadata
    LoRaWANGatewayMetadata (LoRaWANGatewayMetadata'),
    newLoRaWANGatewayMetadata,

    -- ** LoRaWANGatewayVersion
    LoRaWANGatewayVersion (LoRaWANGatewayVersion'),
    newLoRaWANGatewayVersion,

    -- ** LoRaWANGetServiceProfileInfo
    LoRaWANGetServiceProfileInfo (LoRaWANGetServiceProfileInfo'),
    newLoRaWANGetServiceProfileInfo,

    -- ** LoRaWANJoinEventNotificationConfigurations
    LoRaWANJoinEventNotificationConfigurations (LoRaWANJoinEventNotificationConfigurations'),
    newLoRaWANJoinEventNotificationConfigurations,

    -- ** LoRaWANJoinResourceTypeEventConfiguration
    LoRaWANJoinResourceTypeEventConfiguration (LoRaWANJoinResourceTypeEventConfiguration'),
    newLoRaWANJoinResourceTypeEventConfiguration,

    -- ** LoRaWANListDevice
    LoRaWANListDevice (LoRaWANListDevice'),
    newLoRaWANListDevice,

    -- ** LoRaWANMulticast
    LoRaWANMulticast (LoRaWANMulticast'),
    newLoRaWANMulticast,

    -- ** LoRaWANMulticastGet
    LoRaWANMulticastGet (LoRaWANMulticastGet'),
    newLoRaWANMulticastGet,

    -- ** LoRaWANMulticastMetadata
    LoRaWANMulticastMetadata (LoRaWANMulticastMetadata'),
    newLoRaWANMulticastMetadata,

    -- ** LoRaWANMulticastSession
    LoRaWANMulticastSession (LoRaWANMulticastSession'),
    newLoRaWANMulticastSession,

    -- ** LoRaWANSendDataToDevice
    LoRaWANSendDataToDevice (LoRaWANSendDataToDevice'),
    newLoRaWANSendDataToDevice,

    -- ** LoRaWANServiceProfile
    LoRaWANServiceProfile (LoRaWANServiceProfile'),
    newLoRaWANServiceProfile,

    -- ** LoRaWANStartFuotaTask
    LoRaWANStartFuotaTask (LoRaWANStartFuotaTask'),
    newLoRaWANStartFuotaTask,

    -- ** LoRaWANUpdateDevice
    LoRaWANUpdateDevice (LoRaWANUpdateDevice'),
    newLoRaWANUpdateDevice,

    -- ** LoRaWANUpdateGatewayTaskCreate
    LoRaWANUpdateGatewayTaskCreate (LoRaWANUpdateGatewayTaskCreate'),
    newLoRaWANUpdateGatewayTaskCreate,

    -- ** LoRaWANUpdateGatewayTaskEntry
    LoRaWANUpdateGatewayTaskEntry (LoRaWANUpdateGatewayTaskEntry'),
    newLoRaWANUpdateGatewayTaskEntry,

    -- ** LteLocalId
    LteLocalId (LteLocalId'),
    newLteLocalId,

    -- ** LteNmrObj
    LteNmrObj (LteNmrObj'),
    newLteNmrObj,

    -- ** LteObj
    LteObj (LteObj'),
    newLteObj,

    -- ** MessageDeliveryStatusEventConfiguration
    MessageDeliveryStatusEventConfiguration (MessageDeliveryStatusEventConfiguration'),
    newMessageDeliveryStatusEventConfiguration,

    -- ** MessageDeliveryStatusResourceTypeEventConfiguration
    MessageDeliveryStatusResourceTypeEventConfiguration (MessageDeliveryStatusResourceTypeEventConfiguration'),
    newMessageDeliveryStatusResourceTypeEventConfiguration,

    -- ** MulticastGroup
    MulticastGroup (MulticastGroup'),
    newMulticastGroup,

    -- ** MulticastGroupByFuotaTask
    MulticastGroupByFuotaTask (MulticastGroupByFuotaTask'),
    newMulticastGroupByFuotaTask,

    -- ** MulticastWirelessMetadata
    MulticastWirelessMetadata (MulticastWirelessMetadata'),
    newMulticastWirelessMetadata,

    -- ** NetworkAnalyzerConfigurations
    NetworkAnalyzerConfigurations (NetworkAnalyzerConfigurations'),
    newNetworkAnalyzerConfigurations,

    -- ** OtaaV1_0_x
    OtaaV1_0_x (OtaaV1_0_x'),
    newOtaaV1_0_x,

    -- ** OtaaV1_1
    OtaaV1_1 (OtaaV1_1'),
    newOtaaV1_1,

    -- ** ParticipatingGateways
    ParticipatingGateways (ParticipatingGateways'),
    newParticipatingGateways,

    -- ** Positioning
    Positioning (Positioning'),
    newPositioning,

    -- ** ProximityEventConfiguration
    ProximityEventConfiguration (ProximityEventConfiguration'),
    newProximityEventConfiguration,

    -- ** ProximityResourceTypeEventConfiguration
    ProximityResourceTypeEventConfiguration (ProximityResourceTypeEventConfiguration'),
    newProximityResourceTypeEventConfiguration,

    -- ** ServiceProfile
    ServiceProfile (ServiceProfile'),
    newServiceProfile,

    -- ** SessionKeysAbpV1_0_x
    SessionKeysAbpV1_0_x (SessionKeysAbpV1_0_x'),
    newSessionKeysAbpV1_0_x,

    -- ** SessionKeysAbpV1_1
    SessionKeysAbpV1_1 (SessionKeysAbpV1_1'),
    newSessionKeysAbpV1_1,

    -- ** SidewalkAccountInfo
    SidewalkAccountInfo (SidewalkAccountInfo'),
    newSidewalkAccountInfo,

    -- ** SidewalkAccountInfoWithFingerprint
    SidewalkAccountInfoWithFingerprint (SidewalkAccountInfoWithFingerprint'),
    newSidewalkAccountInfoWithFingerprint,

    -- ** SidewalkDevice
    SidewalkDevice (SidewalkDevice'),
    newSidewalkDevice,

    -- ** SidewalkDeviceMetadata
    SidewalkDeviceMetadata (SidewalkDeviceMetadata'),
    newSidewalkDeviceMetadata,

    -- ** SidewalkEventNotificationConfigurations
    SidewalkEventNotificationConfigurations (SidewalkEventNotificationConfigurations'),
    newSidewalkEventNotificationConfigurations,

    -- ** SidewalkListDevice
    SidewalkListDevice (SidewalkListDevice'),
    newSidewalkListDevice,

    -- ** SidewalkResourceTypeEventConfiguration
    SidewalkResourceTypeEventConfiguration (SidewalkResourceTypeEventConfiguration'),
    newSidewalkResourceTypeEventConfiguration,

    -- ** SidewalkSendDataToDevice
    SidewalkSendDataToDevice (SidewalkSendDataToDevice'),
    newSidewalkSendDataToDevice,

    -- ** SidewalkUpdateAccount
    SidewalkUpdateAccount (SidewalkUpdateAccount'),
    newSidewalkUpdateAccount,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TdscdmaLocalId
    TdscdmaLocalId (TdscdmaLocalId'),
    newTdscdmaLocalId,

    -- ** TdscdmaNmrObj
    TdscdmaNmrObj (TdscdmaNmrObj'),
    newTdscdmaNmrObj,

    -- ** TdscdmaObj
    TdscdmaObj (TdscdmaObj'),
    newTdscdmaObj,

    -- ** TraceContent
    TraceContent (TraceContent'),
    newTraceContent,

    -- ** UpdateAbpV1_0_x
    UpdateAbpV1_0_x (UpdateAbpV1_0_x'),
    newUpdateAbpV1_0_x,

    -- ** UpdateAbpV1_1
    UpdateAbpV1_1 (UpdateAbpV1_1'),
    newUpdateAbpV1_1,

    -- ** UpdateFPorts
    UpdateFPorts (UpdateFPorts'),
    newUpdateFPorts,

    -- ** UpdateWirelessGatewayTaskCreate
    UpdateWirelessGatewayTaskCreate (UpdateWirelessGatewayTaskCreate'),
    newUpdateWirelessGatewayTaskCreate,

    -- ** UpdateWirelessGatewayTaskEntry
    UpdateWirelessGatewayTaskEntry (UpdateWirelessGatewayTaskEntry'),
    newUpdateWirelessGatewayTaskEntry,

    -- ** WcdmaLocalId
    WcdmaLocalId (WcdmaLocalId'),
    newWcdmaLocalId,

    -- ** WcdmaNmrObj
    WcdmaNmrObj (WcdmaNmrObj'),
    newWcdmaNmrObj,

    -- ** WcdmaObj
    WcdmaObj (WcdmaObj'),
    newWcdmaObj,

    -- ** WiFiAccessPoint
    WiFiAccessPoint (WiFiAccessPoint'),
    newWiFiAccessPoint,

    -- ** WirelessDeviceEventLogOption
    WirelessDeviceEventLogOption (WirelessDeviceEventLogOption'),
    newWirelessDeviceEventLogOption,

    -- ** WirelessDeviceLogOption
    WirelessDeviceLogOption (WirelessDeviceLogOption'),
    newWirelessDeviceLogOption,

    -- ** WirelessDeviceStatistics
    WirelessDeviceStatistics (WirelessDeviceStatistics'),
    newWirelessDeviceStatistics,

    -- ** WirelessGatewayEventLogOption
    WirelessGatewayEventLogOption (WirelessGatewayEventLogOption'),
    newWirelessGatewayEventLogOption,

    -- ** WirelessGatewayLogOption
    WirelessGatewayLogOption (WirelessGatewayLogOption'),
    newWirelessGatewayLogOption,

    -- ** WirelessGatewayStatistics
    WirelessGatewayStatistics (WirelessGatewayStatistics'),
    newWirelessGatewayStatistics,

    -- ** WirelessMetadata
    WirelessMetadata (WirelessMetadata'),
    newWirelessMetadata,
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
import Amazonka.IoTWireless.DeleteWirelessGateway
import Amazonka.IoTWireless.DeleteWirelessGatewayTask
import Amazonka.IoTWireless.DeleteWirelessGatewayTaskDefinition
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
import Amazonka.IoTWireless.GetWirelessDeviceStatistics
import Amazonka.IoTWireless.GetWirelessGateway
import Amazonka.IoTWireless.GetWirelessGatewayCertificate
import Amazonka.IoTWireless.GetWirelessGatewayFirmwareInformation
import Amazonka.IoTWireless.GetWirelessGatewayStatistics
import Amazonka.IoTWireless.GetWirelessGatewayTask
import Amazonka.IoTWireless.GetWirelessGatewayTaskDefinition
import Amazonka.IoTWireless.Lens
import Amazonka.IoTWireless.ListDestinations
import Amazonka.IoTWireless.ListDeviceProfiles
import Amazonka.IoTWireless.ListEventConfigurations
import Amazonka.IoTWireless.ListFuotaTasks
import Amazonka.IoTWireless.ListMulticastGroups
import Amazonka.IoTWireless.ListMulticastGroupsByFuotaTask
import Amazonka.IoTWireless.ListNetworkAnalyzerConfigurations
import Amazonka.IoTWireless.ListPartnerAccounts
import Amazonka.IoTWireless.ListQueuedMessages
import Amazonka.IoTWireless.ListServiceProfiles
import Amazonka.IoTWireless.ListTagsForResource
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
import Amazonka.IoTWireless.TagResource
import Amazonka.IoTWireless.TestWirelessDevice
import Amazonka.IoTWireless.Types
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
import Amazonka.IoTWireless.UpdateWirelessGateway
import Amazonka.IoTWireless.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTWireless'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
