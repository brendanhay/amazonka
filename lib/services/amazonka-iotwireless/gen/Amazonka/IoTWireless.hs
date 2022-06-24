{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTWireless
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-22@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Wireless API documentation
module Amazonka.IoTWireless
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

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

    -- ** GetLogLevelsByResourceTypes
    GetLogLevelsByResourceTypes (GetLogLevelsByResourceTypes'),
    newGetLogLevelsByResourceTypes,
    GetLogLevelsByResourceTypesResponse (GetLogLevelsByResourceTypesResponse'),
    newGetLogLevelsByResourceTypesResponse,

    -- ** GetPartnerAccount
    GetPartnerAccount (GetPartnerAccount'),
    newGetPartnerAccount,
    GetPartnerAccountResponse (GetPartnerAccountResponse'),
    newGetPartnerAccountResponse,

    -- ** GetResourceLogLevel
    GetResourceLogLevel (GetResourceLogLevel'),
    newGetResourceLogLevel,
    GetResourceLogLevelResponse (GetResourceLogLevelResponse'),
    newGetResourceLogLevelResponse,

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

    -- ** ListPartnerAccounts
    ListPartnerAccounts (ListPartnerAccounts'),
    newListPartnerAccounts,
    ListPartnerAccountsResponse (ListPartnerAccountsResponse'),
    newListPartnerAccountsResponse,

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

    -- ** SendDataToWirelessDevice
    SendDataToWirelessDevice (SendDataToWirelessDevice'),
    newSendDataToWirelessDevice,
    SendDataToWirelessDeviceResponse (SendDataToWirelessDeviceResponse'),
    newSendDataToWirelessDeviceResponse,

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

    -- ** UpdateLogLevelsByResourceTypes
    UpdateLogLevelsByResourceTypes (UpdateLogLevelsByResourceTypes'),
    newUpdateLogLevelsByResourceTypes,
    UpdateLogLevelsByResourceTypesResponse (UpdateLogLevelsByResourceTypesResponse'),
    newUpdateLogLevelsByResourceTypesResponse,

    -- ** UpdatePartnerAccount
    UpdatePartnerAccount (UpdatePartnerAccount'),
    newUpdatePartnerAccount,
    UpdatePartnerAccountResponse (UpdatePartnerAccountResponse'),
    newUpdatePartnerAccountResponse,

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
import Amazonka.IoTWireless.Lens
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
import Amazonka.IoTWireless.Types
import Amazonka.IoTWireless.UntagResource
import Amazonka.IoTWireless.UpdateDestination
import Amazonka.IoTWireless.UpdateLogLevelsByResourceTypes
import Amazonka.IoTWireless.UpdatePartnerAccount
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
