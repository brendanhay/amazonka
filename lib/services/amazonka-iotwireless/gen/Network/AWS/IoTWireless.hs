{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.IoTWireless
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-22@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Wireless API documentation
module Network.AWS.IoTWireless
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateWirelessGatewayTaskDefinition
    CreateWirelessGatewayTaskDefinition (CreateWirelessGatewayTaskDefinition'),
    newCreateWirelessGatewayTaskDefinition,
    CreateWirelessGatewayTaskDefinitionResponse (CreateWirelessGatewayTaskDefinitionResponse'),
    newCreateWirelessGatewayTaskDefinitionResponse,

    -- ** ResetAllResourceLogLevels
    ResetAllResourceLogLevels (ResetAllResourceLogLevels'),
    newResetAllResourceLogLevels,
    ResetAllResourceLogLevelsResponse (ResetAllResourceLogLevelsResponse'),
    newResetAllResourceLogLevelsResponse,

    -- ** ListServiceProfiles
    ListServiceProfiles (ListServiceProfiles'),
    newListServiceProfiles,
    ListServiceProfilesResponse (ListServiceProfilesResponse'),
    newListServiceProfilesResponse,

    -- ** DeleteServiceProfile
    DeleteServiceProfile (DeleteServiceProfile'),
    newDeleteServiceProfile,
    DeleteServiceProfileResponse (DeleteServiceProfileResponse'),
    newDeleteServiceProfileResponse,

    -- ** ListDeviceProfiles
    ListDeviceProfiles (ListDeviceProfiles'),
    newListDeviceProfiles,
    ListDeviceProfilesResponse (ListDeviceProfilesResponse'),
    newListDeviceProfilesResponse,

    -- ** AssociateWirelessGatewayWithThing
    AssociateWirelessGatewayWithThing (AssociateWirelessGatewayWithThing'),
    newAssociateWirelessGatewayWithThing,
    AssociateWirelessGatewayWithThingResponse (AssociateWirelessGatewayWithThingResponse'),
    newAssociateWirelessGatewayWithThingResponse,

    -- ** DeleteDeviceProfile
    DeleteDeviceProfile (DeleteDeviceProfile'),
    newDeleteDeviceProfile,
    DeleteDeviceProfileResponse (DeleteDeviceProfileResponse'),
    newDeleteDeviceProfileResponse,

    -- ** AssociateWirelessDeviceWithThing
    AssociateWirelessDeviceWithThing (AssociateWirelessDeviceWithThing'),
    newAssociateWirelessDeviceWithThing,
    AssociateWirelessDeviceWithThingResponse (AssociateWirelessDeviceWithThingResponse'),
    newAssociateWirelessDeviceWithThingResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateServiceProfile
    CreateServiceProfile (CreateServiceProfile'),
    newCreateServiceProfile,
    CreateServiceProfileResponse (CreateServiceProfileResponse'),
    newCreateServiceProfileResponse,

    -- ** AssociateAwsAccountWithPartnerAccount
    AssociateAwsAccountWithPartnerAccount (AssociateAwsAccountWithPartnerAccount'),
    newAssociateAwsAccountWithPartnerAccount,
    AssociateAwsAccountWithPartnerAccountResponse (AssociateAwsAccountWithPartnerAccountResponse'),
    newAssociateAwsAccountWithPartnerAccountResponse,

    -- ** ResetResourceLogLevel
    ResetResourceLogLevel (ResetResourceLogLevel'),
    newResetResourceLogLevel,
    ResetResourceLogLevelResponse (ResetResourceLogLevelResponse'),
    newResetResourceLogLevelResponse,

    -- ** DeleteDestination
    DeleteDestination (DeleteDestination'),
    newDeleteDestination,
    DeleteDestinationResponse (DeleteDestinationResponse'),
    newDeleteDestinationResponse,

    -- ** UpdateDestination
    UpdateDestination (UpdateDestination'),
    newUpdateDestination,
    UpdateDestinationResponse (UpdateDestinationResponse'),
    newUpdateDestinationResponse,

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

    -- ** GetWirelessDeviceStatistics
    GetWirelessDeviceStatistics (GetWirelessDeviceStatistics'),
    newGetWirelessDeviceStatistics,
    GetWirelessDeviceStatisticsResponse (GetWirelessDeviceStatisticsResponse'),
    newGetWirelessDeviceStatisticsResponse,

    -- ** GetWirelessGatewayStatistics
    GetWirelessGatewayStatistics (GetWirelessGatewayStatistics'),
    newGetWirelessGatewayStatistics,
    GetWirelessGatewayStatisticsResponse (GetWirelessGatewayStatisticsResponse'),
    newGetWirelessGatewayStatisticsResponse,

    -- ** GetWirelessGatewayCertificate
    GetWirelessGatewayCertificate (GetWirelessGatewayCertificate'),
    newGetWirelessGatewayCertificate,
    GetWirelessGatewayCertificateResponse (GetWirelessGatewayCertificateResponse'),
    newGetWirelessGatewayCertificateResponse,

    -- ** GetWirelessGatewayTask
    GetWirelessGatewayTask (GetWirelessGatewayTask'),
    newGetWirelessGatewayTask,
    GetWirelessGatewayTaskResponse (GetWirelessGatewayTaskResponse'),
    newGetWirelessGatewayTaskResponse,

    -- ** GetWirelessGatewayFirmwareInformation
    GetWirelessGatewayFirmwareInformation (GetWirelessGatewayFirmwareInformation'),
    newGetWirelessGatewayFirmwareInformation,
    GetWirelessGatewayFirmwareInformationResponse (GetWirelessGatewayFirmwareInformationResponse'),
    newGetWirelessGatewayFirmwareInformationResponse,

    -- ** ListPartnerAccounts
    ListPartnerAccounts (ListPartnerAccounts'),
    newListPartnerAccounts,
    ListPartnerAccountsResponse (ListPartnerAccountsResponse'),
    newListPartnerAccountsResponse,

    -- ** TestWirelessDevice
    TestWirelessDevice (TestWirelessDevice'),
    newTestWirelessDevice,
    TestWirelessDeviceResponse (TestWirelessDeviceResponse'),
    newTestWirelessDeviceResponse,

    -- ** GetWirelessDevice
    GetWirelessDevice (GetWirelessDevice'),
    newGetWirelessDevice,
    GetWirelessDeviceResponse (GetWirelessDeviceResponse'),
    newGetWirelessDeviceResponse,

    -- ** GetWirelessGateway
    GetWirelessGateway (GetWirelessGateway'),
    newGetWirelessGateway,
    GetWirelessGatewayResponse (GetWirelessGatewayResponse'),
    newGetWirelessGatewayResponse,

    -- ** GetPartnerAccount
    GetPartnerAccount (GetPartnerAccount'),
    newGetPartnerAccount,
    GetPartnerAccountResponse (GetPartnerAccountResponse'),
    newGetPartnerAccountResponse,

    -- ** CreateWirelessGatewayTask
    CreateWirelessGatewayTask (CreateWirelessGatewayTask'),
    newCreateWirelessGatewayTask,
    CreateWirelessGatewayTaskResponse (CreateWirelessGatewayTaskResponse'),
    newCreateWirelessGatewayTaskResponse,

    -- ** DeleteWirelessGateway
    DeleteWirelessGateway (DeleteWirelessGateway'),
    newDeleteWirelessGateway,
    DeleteWirelessGatewayResponse (DeleteWirelessGatewayResponse'),
    newDeleteWirelessGatewayResponse,

    -- ** UpdateWirelessGateway
    UpdateWirelessGateway (UpdateWirelessGateway'),
    newUpdateWirelessGateway,
    UpdateWirelessGatewayResponse (UpdateWirelessGatewayResponse'),
    newUpdateWirelessGatewayResponse,

    -- ** DisassociateWirelessGatewayFromCertificate
    DisassociateWirelessGatewayFromCertificate (DisassociateWirelessGatewayFromCertificate'),
    newDisassociateWirelessGatewayFromCertificate,
    DisassociateWirelessGatewayFromCertificateResponse (DisassociateWirelessGatewayFromCertificateResponse'),
    newDisassociateWirelessGatewayFromCertificateResponse,

    -- ** ListWirelessGatewayTaskDefinitions
    ListWirelessGatewayTaskDefinitions (ListWirelessGatewayTaskDefinitions'),
    newListWirelessGatewayTaskDefinitions,
    ListWirelessGatewayTaskDefinitionsResponse (ListWirelessGatewayTaskDefinitionsResponse'),
    newListWirelessGatewayTaskDefinitionsResponse,

    -- ** PutResourceLogLevel
    PutResourceLogLevel (PutResourceLogLevel'),
    newPutResourceLogLevel,
    PutResourceLogLevelResponse (PutResourceLogLevelResponse'),
    newPutResourceLogLevelResponse,

    -- ** CreateWirelessGateway
    CreateWirelessGateway (CreateWirelessGateway'),
    newCreateWirelessGateway,
    CreateWirelessGatewayResponse (CreateWirelessGatewayResponse'),
    newCreateWirelessGatewayResponse,

    -- ** DeleteWirelessGatewayTask
    DeleteWirelessGatewayTask (DeleteWirelessGatewayTask'),
    newDeleteWirelessGatewayTask,
    DeleteWirelessGatewayTaskResponse (DeleteWirelessGatewayTaskResponse'),
    newDeleteWirelessGatewayTaskResponse,

    -- ** CreateWirelessDevice
    CreateWirelessDevice (CreateWirelessDevice'),
    newCreateWirelessDevice,
    CreateWirelessDeviceResponse (CreateWirelessDeviceResponse'),
    newCreateWirelessDeviceResponse,

    -- ** SendDataToWirelessDevice
    SendDataToWirelessDevice (SendDataToWirelessDevice'),
    newSendDataToWirelessDevice,
    SendDataToWirelessDeviceResponse (SendDataToWirelessDeviceResponse'),
    newSendDataToWirelessDeviceResponse,

    -- ** CreateDeviceProfile
    CreateDeviceProfile (CreateDeviceProfile'),
    newCreateDeviceProfile,
    CreateDeviceProfileResponse (CreateDeviceProfileResponse'),
    newCreateDeviceProfileResponse,

    -- ** DisassociateAwsAccountFromPartnerAccount
    DisassociateAwsAccountFromPartnerAccount (DisassociateAwsAccountFromPartnerAccount'),
    newDisassociateAwsAccountFromPartnerAccount,
    DisassociateAwsAccountFromPartnerAccountResponse (DisassociateAwsAccountFromPartnerAccountResponse'),
    newDisassociateAwsAccountFromPartnerAccountResponse,

    -- ** UpdateLogLevelsByResourceTypes
    UpdateLogLevelsByResourceTypes (UpdateLogLevelsByResourceTypes'),
    newUpdateLogLevelsByResourceTypes,
    UpdateLogLevelsByResourceTypesResponse (UpdateLogLevelsByResourceTypesResponse'),
    newUpdateLogLevelsByResourceTypesResponse,

    -- ** GetDestination
    GetDestination (GetDestination'),
    newGetDestination,
    GetDestinationResponse (GetDestinationResponse'),
    newGetDestinationResponse,

    -- ** GetLogLevelsByResourceTypes
    GetLogLevelsByResourceTypes (GetLogLevelsByResourceTypes'),
    newGetLogLevelsByResourceTypes,
    GetLogLevelsByResourceTypesResponse (GetLogLevelsByResourceTypesResponse'),
    newGetLogLevelsByResourceTypesResponse,

    -- ** ListDestinations
    ListDestinations (ListDestinations'),
    newListDestinations,
    ListDestinationsResponse (ListDestinationsResponse'),
    newListDestinationsResponse,

    -- ** GetDeviceProfile
    GetDeviceProfile (GetDeviceProfile'),
    newGetDeviceProfile,
    GetDeviceProfileResponse (GetDeviceProfileResponse'),
    newGetDeviceProfileResponse,

    -- ** DisassociateWirelessDeviceFromThing
    DisassociateWirelessDeviceFromThing (DisassociateWirelessDeviceFromThing'),
    newDisassociateWirelessDeviceFromThing,
    DisassociateWirelessDeviceFromThingResponse (DisassociateWirelessDeviceFromThingResponse'),
    newDisassociateWirelessDeviceFromThingResponse,

    -- ** DisassociateWirelessGatewayFromThing
    DisassociateWirelessGatewayFromThing (DisassociateWirelessGatewayFromThing'),
    newDisassociateWirelessGatewayFromThing,
    DisassociateWirelessGatewayFromThingResponse (DisassociateWirelessGatewayFromThingResponse'),
    newDisassociateWirelessGatewayFromThingResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateDestination
    CreateDestination (CreateDestination'),
    newCreateDestination,
    CreateDestinationResponse (CreateDestinationResponse'),
    newCreateDestinationResponse,

    -- ** AssociateWirelessGatewayWithCertificate
    AssociateWirelessGatewayWithCertificate (AssociateWirelessGatewayWithCertificate'),
    newAssociateWirelessGatewayWithCertificate,
    AssociateWirelessGatewayWithCertificateResponse (AssociateWirelessGatewayWithCertificateResponse'),
    newAssociateWirelessGatewayWithCertificateResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdatePartnerAccount
    UpdatePartnerAccount (UpdatePartnerAccount'),
    newUpdatePartnerAccount,
    UpdatePartnerAccountResponse (UpdatePartnerAccountResponse'),
    newUpdatePartnerAccountResponse,

    -- ** GetWirelessGatewayTaskDefinition
    GetWirelessGatewayTaskDefinition (GetWirelessGatewayTaskDefinition'),
    newGetWirelessGatewayTaskDefinition,
    GetWirelessGatewayTaskDefinitionResponse (GetWirelessGatewayTaskDefinitionResponse'),
    newGetWirelessGatewayTaskDefinitionResponse,

    -- ** GetResourceLogLevel
    GetResourceLogLevel (GetResourceLogLevel'),
    newGetResourceLogLevel,
    GetResourceLogLevelResponse (GetResourceLogLevelResponse'),
    newGetResourceLogLevelResponse,

    -- ** DeleteWirelessDevice
    DeleteWirelessDevice (DeleteWirelessDevice'),
    newDeleteWirelessDevice,
    DeleteWirelessDeviceResponse (DeleteWirelessDeviceResponse'),
    newDeleteWirelessDeviceResponse,

    -- ** UpdateWirelessDevice
    UpdateWirelessDevice (UpdateWirelessDevice'),
    newUpdateWirelessDevice,
    UpdateWirelessDeviceResponse (UpdateWirelessDeviceResponse'),
    newUpdateWirelessDeviceResponse,

    -- ** ListWirelessGateways
    ListWirelessGateways (ListWirelessGateways'),
    newListWirelessGateways,
    ListWirelessGatewaysResponse (ListWirelessGatewaysResponse'),
    newListWirelessGatewaysResponse,

    -- ** DeleteWirelessGatewayTaskDefinition
    DeleteWirelessGatewayTaskDefinition (DeleteWirelessGatewayTaskDefinition'),
    newDeleteWirelessGatewayTaskDefinition,
    DeleteWirelessGatewayTaskDefinitionResponse (DeleteWirelessGatewayTaskDefinitionResponse'),
    newDeleteWirelessGatewayTaskDefinitionResponse,

    -- ** ListWirelessDevices
    ListWirelessDevices (ListWirelessDevices'),
    newListWirelessDevices,
    ListWirelessDevicesResponse (ListWirelessDevicesResponse'),
    newListWirelessDevicesResponse,

    -- * Types

    -- ** BatteryLevel
    BatteryLevel (..),

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** DeviceState
    DeviceState (..),

    -- ** Event
    Event (..),

    -- ** ExpressionType
    ExpressionType (..),

    -- ** LogLevel
    LogLevel (..),

    -- ** MessageType
    MessageType (..),

    -- ** PartnerType
    PartnerType (..),

    -- ** SigningAlg
    SigningAlg (..),

    -- ** WirelessDeviceEvent
    WirelessDeviceEvent (..),

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

    -- ** CertificateList
    CertificateList (CertificateList'),
    newCertificateList,

    -- ** Destinations
    Destinations (Destinations'),
    newDestinations,

    -- ** DeviceProfile
    DeviceProfile (DeviceProfile'),
    newDeviceProfile,

    -- ** LoRaWANDevice
    LoRaWANDevice (LoRaWANDevice'),
    newLoRaWANDevice,

    -- ** LoRaWANDeviceMetadata
    LoRaWANDeviceMetadata (LoRaWANDeviceMetadata'),
    newLoRaWANDeviceMetadata,

    -- ** LoRaWANDeviceProfile
    LoRaWANDeviceProfile (LoRaWANDeviceProfile'),
    newLoRaWANDeviceProfile,

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

    -- ** LoRaWANListDevice
    LoRaWANListDevice (LoRaWANListDevice'),
    newLoRaWANListDevice,

    -- ** LoRaWANSendDataToDevice
    LoRaWANSendDataToDevice (LoRaWANSendDataToDevice'),
    newLoRaWANSendDataToDevice,

    -- ** LoRaWANServiceProfile
    LoRaWANServiceProfile (LoRaWANServiceProfile'),
    newLoRaWANServiceProfile,

    -- ** LoRaWANUpdateDevice
    LoRaWANUpdateDevice (LoRaWANUpdateDevice'),
    newLoRaWANUpdateDevice,

    -- ** LoRaWANUpdateGatewayTaskCreate
    LoRaWANUpdateGatewayTaskCreate (LoRaWANUpdateGatewayTaskCreate'),
    newLoRaWANUpdateGatewayTaskCreate,

    -- ** LoRaWANUpdateGatewayTaskEntry
    LoRaWANUpdateGatewayTaskEntry (LoRaWANUpdateGatewayTaskEntry'),
    newLoRaWANUpdateGatewayTaskEntry,

    -- ** OtaaV1_0_x
    OtaaV1_0_x (OtaaV1_0_x'),
    newOtaaV1_0_x,

    -- ** OtaaV1_1
    OtaaV1_1 (OtaaV1_1'),
    newOtaaV1_1,

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

    -- ** SidewalkListDevice
    SidewalkListDevice (SidewalkListDevice'),
    newSidewalkListDevice,

    -- ** SidewalkSendDataToDevice
    SidewalkSendDataToDevice (SidewalkSendDataToDevice'),
    newSidewalkSendDataToDevice,

    -- ** SidewalkUpdateAccount
    SidewalkUpdateAccount (SidewalkUpdateAccount'),
    newSidewalkUpdateAccount,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UpdateWirelessGatewayTaskCreate
    UpdateWirelessGatewayTaskCreate (UpdateWirelessGatewayTaskCreate'),
    newUpdateWirelessGatewayTaskCreate,

    -- ** UpdateWirelessGatewayTaskEntry
    UpdateWirelessGatewayTaskEntry (UpdateWirelessGatewayTaskEntry'),
    newUpdateWirelessGatewayTaskEntry,

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
import Network.AWS.IoTWireless.Lens
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
import Network.AWS.IoTWireless.Types
import Network.AWS.IoTWireless.UntagResource
import Network.AWS.IoTWireless.UpdateDestination
import Network.AWS.IoTWireless.UpdateLogLevelsByResourceTypes
import Network.AWS.IoTWireless.UpdatePartnerAccount
import Network.AWS.IoTWireless.UpdateWirelessDevice
import Network.AWS.IoTWireless.UpdateWirelessGateway
import Network.AWS.IoTWireless.Waiters

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
