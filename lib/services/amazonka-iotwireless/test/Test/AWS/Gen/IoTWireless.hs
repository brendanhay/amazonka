{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTWireless
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoTWireless where

import Amazonka.IoTWireless
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.IoTWireless.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateWirelessGatewayTaskDefinition $
--             newCreateWirelessGatewayTaskDefinition
--
--         , requestResetAllResourceLogLevels $
--             newResetAllResourceLogLevels
--
--         , requestListServiceProfiles $
--             newListServiceProfiles
--
--         , requestDeleteServiceProfile $
--             newDeleteServiceProfile
--
--         , requestListDeviceProfiles $
--             newListDeviceProfiles
--
--         , requestAssociateWirelessGatewayWithThing $
--             newAssociateWirelessGatewayWithThing
--
--         , requestDeleteDeviceProfile $
--             newDeleteDeviceProfile
--
--         , requestAssociateWirelessDeviceWithThing $
--             newAssociateWirelessDeviceWithThing
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateServiceProfile $
--             newCreateServiceProfile
--
--         , requestAssociateAwsAccountWithPartnerAccount $
--             newAssociateAwsAccountWithPartnerAccount
--
--         , requestResetResourceLogLevel $
--             newResetResourceLogLevel
--
--         , requestDeleteDestination $
--             newDeleteDestination
--
--         , requestUpdateDestination $
--             newUpdateDestination
--
--         , requestGetServiceEndpoint $
--             newGetServiceEndpoint
--
--         , requestGetServiceProfile $
--             newGetServiceProfile
--
--         , requestGetWirelessDeviceStatistics $
--             newGetWirelessDeviceStatistics
--
--         , requestGetWirelessGatewayStatistics $
--             newGetWirelessGatewayStatistics
--
--         , requestGetWirelessGatewayCertificate $
--             newGetWirelessGatewayCertificate
--
--         , requestGetWirelessGatewayTask $
--             newGetWirelessGatewayTask
--
--         , requestGetWirelessGatewayFirmwareInformation $
--             newGetWirelessGatewayFirmwareInformation
--
--         , requestListPartnerAccounts $
--             newListPartnerAccounts
--
--         , requestTestWirelessDevice $
--             newTestWirelessDevice
--
--         , requestGetWirelessDevice $
--             newGetWirelessDevice
--
--         , requestGetWirelessGateway $
--             newGetWirelessGateway
--
--         , requestGetPartnerAccount $
--             newGetPartnerAccount
--
--         , requestCreateWirelessGatewayTask $
--             newCreateWirelessGatewayTask
--
--         , requestDeleteWirelessGateway $
--             newDeleteWirelessGateway
--
--         , requestUpdateWirelessGateway $
--             newUpdateWirelessGateway
--
--         , requestDisassociateWirelessGatewayFromCertificate $
--             newDisassociateWirelessGatewayFromCertificate
--
--         , requestListWirelessGatewayTaskDefinitions $
--             newListWirelessGatewayTaskDefinitions
--
--         , requestPutResourceLogLevel $
--             newPutResourceLogLevel
--
--         , requestCreateWirelessGateway $
--             newCreateWirelessGateway
--
--         , requestDeleteWirelessGatewayTask $
--             newDeleteWirelessGatewayTask
--
--         , requestCreateWirelessDevice $
--             newCreateWirelessDevice
--
--         , requestSendDataToWirelessDevice $
--             newSendDataToWirelessDevice
--
--         , requestCreateDeviceProfile $
--             newCreateDeviceProfile
--
--         , requestDisassociateAwsAccountFromPartnerAccount $
--             newDisassociateAwsAccountFromPartnerAccount
--
--         , requestUpdateLogLevelsByResourceTypes $
--             newUpdateLogLevelsByResourceTypes
--
--         , requestGetDestination $
--             newGetDestination
--
--         , requestGetLogLevelsByResourceTypes $
--             newGetLogLevelsByResourceTypes
--
--         , requestListDestinations $
--             newListDestinations
--
--         , requestGetDeviceProfile $
--             newGetDeviceProfile
--
--         , requestDisassociateWirelessDeviceFromThing $
--             newDisassociateWirelessDeviceFromThing
--
--         , requestDisassociateWirelessGatewayFromThing $
--             newDisassociateWirelessGatewayFromThing
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateDestination $
--             newCreateDestination
--
--         , requestAssociateWirelessGatewayWithCertificate $
--             newAssociateWirelessGatewayWithCertificate
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdatePartnerAccount $
--             newUpdatePartnerAccount
--
--         , requestGetWirelessGatewayTaskDefinition $
--             newGetWirelessGatewayTaskDefinition
--
--         , requestGetResourceLogLevel $
--             newGetResourceLogLevel
--
--         , requestDeleteWirelessDevice $
--             newDeleteWirelessDevice
--
--         , requestUpdateWirelessDevice $
--             newUpdateWirelessDevice
--
--         , requestListWirelessGateways $
--             newListWirelessGateways
--
--         , requestDeleteWirelessGatewayTaskDefinition $
--             newDeleteWirelessGatewayTaskDefinition
--
--         , requestListWirelessDevices $
--             newListWirelessDevices
--
--           ]

--     , testGroup "response"
--         [ responseCreateWirelessGatewayTaskDefinition $
--             newCreateWirelessGatewayTaskDefinitionResponse
--
--         , responseResetAllResourceLogLevels $
--             newResetAllResourceLogLevelsResponse
--
--         , responseListServiceProfiles $
--             newListServiceProfilesResponse
--
--         , responseDeleteServiceProfile $
--             newDeleteServiceProfileResponse
--
--         , responseListDeviceProfiles $
--             newListDeviceProfilesResponse
--
--         , responseAssociateWirelessGatewayWithThing $
--             newAssociateWirelessGatewayWithThingResponse
--
--         , responseDeleteDeviceProfile $
--             newDeleteDeviceProfileResponse
--
--         , responseAssociateWirelessDeviceWithThing $
--             newAssociateWirelessDeviceWithThingResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateServiceProfile $
--             newCreateServiceProfileResponse
--
--         , responseAssociateAwsAccountWithPartnerAccount $
--             newAssociateAwsAccountWithPartnerAccountResponse
--
--         , responseResetResourceLogLevel $
--             newResetResourceLogLevelResponse
--
--         , responseDeleteDestination $
--             newDeleteDestinationResponse
--
--         , responseUpdateDestination $
--             newUpdateDestinationResponse
--
--         , responseGetServiceEndpoint $
--             newGetServiceEndpointResponse
--
--         , responseGetServiceProfile $
--             newGetServiceProfileResponse
--
--         , responseGetWirelessDeviceStatistics $
--             newGetWirelessDeviceStatisticsResponse
--
--         , responseGetWirelessGatewayStatistics $
--             newGetWirelessGatewayStatisticsResponse
--
--         , responseGetWirelessGatewayCertificate $
--             newGetWirelessGatewayCertificateResponse
--
--         , responseGetWirelessGatewayTask $
--             newGetWirelessGatewayTaskResponse
--
--         , responseGetWirelessGatewayFirmwareInformation $
--             newGetWirelessGatewayFirmwareInformationResponse
--
--         , responseListPartnerAccounts $
--             newListPartnerAccountsResponse
--
--         , responseTestWirelessDevice $
--             newTestWirelessDeviceResponse
--
--         , responseGetWirelessDevice $
--             newGetWirelessDeviceResponse
--
--         , responseGetWirelessGateway $
--             newGetWirelessGatewayResponse
--
--         , responseGetPartnerAccount $
--             newGetPartnerAccountResponse
--
--         , responseCreateWirelessGatewayTask $
--             newCreateWirelessGatewayTaskResponse
--
--         , responseDeleteWirelessGateway $
--             newDeleteWirelessGatewayResponse
--
--         , responseUpdateWirelessGateway $
--             newUpdateWirelessGatewayResponse
--
--         , responseDisassociateWirelessGatewayFromCertificate $
--             newDisassociateWirelessGatewayFromCertificateResponse
--
--         , responseListWirelessGatewayTaskDefinitions $
--             newListWirelessGatewayTaskDefinitionsResponse
--
--         , responsePutResourceLogLevel $
--             newPutResourceLogLevelResponse
--
--         , responseCreateWirelessGateway $
--             newCreateWirelessGatewayResponse
--
--         , responseDeleteWirelessGatewayTask $
--             newDeleteWirelessGatewayTaskResponse
--
--         , responseCreateWirelessDevice $
--             newCreateWirelessDeviceResponse
--
--         , responseSendDataToWirelessDevice $
--             newSendDataToWirelessDeviceResponse
--
--         , responseCreateDeviceProfile $
--             newCreateDeviceProfileResponse
--
--         , responseDisassociateAwsAccountFromPartnerAccount $
--             newDisassociateAwsAccountFromPartnerAccountResponse
--
--         , responseUpdateLogLevelsByResourceTypes $
--             newUpdateLogLevelsByResourceTypesResponse
--
--         , responseGetDestination $
--             newGetDestinationResponse
--
--         , responseGetLogLevelsByResourceTypes $
--             newGetLogLevelsByResourceTypesResponse
--
--         , responseListDestinations $
--             newListDestinationsResponse
--
--         , responseGetDeviceProfile $
--             newGetDeviceProfileResponse
--
--         , responseDisassociateWirelessDeviceFromThing $
--             newDisassociateWirelessDeviceFromThingResponse
--
--         , responseDisassociateWirelessGatewayFromThing $
--             newDisassociateWirelessGatewayFromThingResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateDestination $
--             newCreateDestinationResponse
--
--         , responseAssociateWirelessGatewayWithCertificate $
--             newAssociateWirelessGatewayWithCertificateResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdatePartnerAccount $
--             newUpdatePartnerAccountResponse
--
--         , responseGetWirelessGatewayTaskDefinition $
--             newGetWirelessGatewayTaskDefinitionResponse
--
--         , responseGetResourceLogLevel $
--             newGetResourceLogLevelResponse
--
--         , responseDeleteWirelessDevice $
--             newDeleteWirelessDeviceResponse
--
--         , responseUpdateWirelessDevice $
--             newUpdateWirelessDeviceResponse
--
--         , responseListWirelessGateways $
--             newListWirelessGatewaysResponse
--
--         , responseDeleteWirelessGatewayTaskDefinition $
--             newDeleteWirelessGatewayTaskDefinitionResponse
--
--         , responseListWirelessDevices $
--             newListWirelessDevicesResponse
--
--           ]
--     ]

-- Requests

requestCreateWirelessGatewayTaskDefinition :: CreateWirelessGatewayTaskDefinition -> TestTree
requestCreateWirelessGatewayTaskDefinition =
  req
    "CreateWirelessGatewayTaskDefinition"
    "fixture/CreateWirelessGatewayTaskDefinition.yaml"

requestResetAllResourceLogLevels :: ResetAllResourceLogLevels -> TestTree
requestResetAllResourceLogLevels =
  req
    "ResetAllResourceLogLevels"
    "fixture/ResetAllResourceLogLevels.yaml"

requestListServiceProfiles :: ListServiceProfiles -> TestTree
requestListServiceProfiles =
  req
    "ListServiceProfiles"
    "fixture/ListServiceProfiles.yaml"

requestDeleteServiceProfile :: DeleteServiceProfile -> TestTree
requestDeleteServiceProfile =
  req
    "DeleteServiceProfile"
    "fixture/DeleteServiceProfile.yaml"

requestListDeviceProfiles :: ListDeviceProfiles -> TestTree
requestListDeviceProfiles =
  req
    "ListDeviceProfiles"
    "fixture/ListDeviceProfiles.yaml"

requestAssociateWirelessGatewayWithThing :: AssociateWirelessGatewayWithThing -> TestTree
requestAssociateWirelessGatewayWithThing =
  req
    "AssociateWirelessGatewayWithThing"
    "fixture/AssociateWirelessGatewayWithThing.yaml"

requestDeleteDeviceProfile :: DeleteDeviceProfile -> TestTree
requestDeleteDeviceProfile =
  req
    "DeleteDeviceProfile"
    "fixture/DeleteDeviceProfile.yaml"

requestAssociateWirelessDeviceWithThing :: AssociateWirelessDeviceWithThing -> TestTree
requestAssociateWirelessDeviceWithThing =
  req
    "AssociateWirelessDeviceWithThing"
    "fixture/AssociateWirelessDeviceWithThing.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateServiceProfile :: CreateServiceProfile -> TestTree
requestCreateServiceProfile =
  req
    "CreateServiceProfile"
    "fixture/CreateServiceProfile.yaml"

requestAssociateAwsAccountWithPartnerAccount :: AssociateAwsAccountWithPartnerAccount -> TestTree
requestAssociateAwsAccountWithPartnerAccount =
  req
    "AssociateAwsAccountWithPartnerAccount"
    "fixture/AssociateAwsAccountWithPartnerAccount.yaml"

requestResetResourceLogLevel :: ResetResourceLogLevel -> TestTree
requestResetResourceLogLevel =
  req
    "ResetResourceLogLevel"
    "fixture/ResetResourceLogLevel.yaml"

requestDeleteDestination :: DeleteDestination -> TestTree
requestDeleteDestination =
  req
    "DeleteDestination"
    "fixture/DeleteDestination.yaml"

requestUpdateDestination :: UpdateDestination -> TestTree
requestUpdateDestination =
  req
    "UpdateDestination"
    "fixture/UpdateDestination.yaml"

requestGetServiceEndpoint :: GetServiceEndpoint -> TestTree
requestGetServiceEndpoint =
  req
    "GetServiceEndpoint"
    "fixture/GetServiceEndpoint.yaml"

requestGetServiceProfile :: GetServiceProfile -> TestTree
requestGetServiceProfile =
  req
    "GetServiceProfile"
    "fixture/GetServiceProfile.yaml"

requestGetWirelessDeviceStatistics :: GetWirelessDeviceStatistics -> TestTree
requestGetWirelessDeviceStatistics =
  req
    "GetWirelessDeviceStatistics"
    "fixture/GetWirelessDeviceStatistics.yaml"

requestGetWirelessGatewayStatistics :: GetWirelessGatewayStatistics -> TestTree
requestGetWirelessGatewayStatistics =
  req
    "GetWirelessGatewayStatistics"
    "fixture/GetWirelessGatewayStatistics.yaml"

requestGetWirelessGatewayCertificate :: GetWirelessGatewayCertificate -> TestTree
requestGetWirelessGatewayCertificate =
  req
    "GetWirelessGatewayCertificate"
    "fixture/GetWirelessGatewayCertificate.yaml"

requestGetWirelessGatewayTask :: GetWirelessGatewayTask -> TestTree
requestGetWirelessGatewayTask =
  req
    "GetWirelessGatewayTask"
    "fixture/GetWirelessGatewayTask.yaml"

requestGetWirelessGatewayFirmwareInformation :: GetWirelessGatewayFirmwareInformation -> TestTree
requestGetWirelessGatewayFirmwareInformation =
  req
    "GetWirelessGatewayFirmwareInformation"
    "fixture/GetWirelessGatewayFirmwareInformation.yaml"

requestListPartnerAccounts :: ListPartnerAccounts -> TestTree
requestListPartnerAccounts =
  req
    "ListPartnerAccounts"
    "fixture/ListPartnerAccounts.yaml"

requestTestWirelessDevice :: TestWirelessDevice -> TestTree
requestTestWirelessDevice =
  req
    "TestWirelessDevice"
    "fixture/TestWirelessDevice.yaml"

requestGetWirelessDevice :: GetWirelessDevice -> TestTree
requestGetWirelessDevice =
  req
    "GetWirelessDevice"
    "fixture/GetWirelessDevice.yaml"

requestGetWirelessGateway :: GetWirelessGateway -> TestTree
requestGetWirelessGateway =
  req
    "GetWirelessGateway"
    "fixture/GetWirelessGateway.yaml"

requestGetPartnerAccount :: GetPartnerAccount -> TestTree
requestGetPartnerAccount =
  req
    "GetPartnerAccount"
    "fixture/GetPartnerAccount.yaml"

requestCreateWirelessGatewayTask :: CreateWirelessGatewayTask -> TestTree
requestCreateWirelessGatewayTask =
  req
    "CreateWirelessGatewayTask"
    "fixture/CreateWirelessGatewayTask.yaml"

requestDeleteWirelessGateway :: DeleteWirelessGateway -> TestTree
requestDeleteWirelessGateway =
  req
    "DeleteWirelessGateway"
    "fixture/DeleteWirelessGateway.yaml"

requestUpdateWirelessGateway :: UpdateWirelessGateway -> TestTree
requestUpdateWirelessGateway =
  req
    "UpdateWirelessGateway"
    "fixture/UpdateWirelessGateway.yaml"

requestDisassociateWirelessGatewayFromCertificate :: DisassociateWirelessGatewayFromCertificate -> TestTree
requestDisassociateWirelessGatewayFromCertificate =
  req
    "DisassociateWirelessGatewayFromCertificate"
    "fixture/DisassociateWirelessGatewayFromCertificate.yaml"

requestListWirelessGatewayTaskDefinitions :: ListWirelessGatewayTaskDefinitions -> TestTree
requestListWirelessGatewayTaskDefinitions =
  req
    "ListWirelessGatewayTaskDefinitions"
    "fixture/ListWirelessGatewayTaskDefinitions.yaml"

requestPutResourceLogLevel :: PutResourceLogLevel -> TestTree
requestPutResourceLogLevel =
  req
    "PutResourceLogLevel"
    "fixture/PutResourceLogLevel.yaml"

requestCreateWirelessGateway :: CreateWirelessGateway -> TestTree
requestCreateWirelessGateway =
  req
    "CreateWirelessGateway"
    "fixture/CreateWirelessGateway.yaml"

requestDeleteWirelessGatewayTask :: DeleteWirelessGatewayTask -> TestTree
requestDeleteWirelessGatewayTask =
  req
    "DeleteWirelessGatewayTask"
    "fixture/DeleteWirelessGatewayTask.yaml"

requestCreateWirelessDevice :: CreateWirelessDevice -> TestTree
requestCreateWirelessDevice =
  req
    "CreateWirelessDevice"
    "fixture/CreateWirelessDevice.yaml"

requestSendDataToWirelessDevice :: SendDataToWirelessDevice -> TestTree
requestSendDataToWirelessDevice =
  req
    "SendDataToWirelessDevice"
    "fixture/SendDataToWirelessDevice.yaml"

requestCreateDeviceProfile :: CreateDeviceProfile -> TestTree
requestCreateDeviceProfile =
  req
    "CreateDeviceProfile"
    "fixture/CreateDeviceProfile.yaml"

requestDisassociateAwsAccountFromPartnerAccount :: DisassociateAwsAccountFromPartnerAccount -> TestTree
requestDisassociateAwsAccountFromPartnerAccount =
  req
    "DisassociateAwsAccountFromPartnerAccount"
    "fixture/DisassociateAwsAccountFromPartnerAccount.yaml"

requestUpdateLogLevelsByResourceTypes :: UpdateLogLevelsByResourceTypes -> TestTree
requestUpdateLogLevelsByResourceTypes =
  req
    "UpdateLogLevelsByResourceTypes"
    "fixture/UpdateLogLevelsByResourceTypes.yaml"

requestGetDestination :: GetDestination -> TestTree
requestGetDestination =
  req
    "GetDestination"
    "fixture/GetDestination.yaml"

requestGetLogLevelsByResourceTypes :: GetLogLevelsByResourceTypes -> TestTree
requestGetLogLevelsByResourceTypes =
  req
    "GetLogLevelsByResourceTypes"
    "fixture/GetLogLevelsByResourceTypes.yaml"

requestListDestinations :: ListDestinations -> TestTree
requestListDestinations =
  req
    "ListDestinations"
    "fixture/ListDestinations.yaml"

requestGetDeviceProfile :: GetDeviceProfile -> TestTree
requestGetDeviceProfile =
  req
    "GetDeviceProfile"
    "fixture/GetDeviceProfile.yaml"

requestDisassociateWirelessDeviceFromThing :: DisassociateWirelessDeviceFromThing -> TestTree
requestDisassociateWirelessDeviceFromThing =
  req
    "DisassociateWirelessDeviceFromThing"
    "fixture/DisassociateWirelessDeviceFromThing.yaml"

requestDisassociateWirelessGatewayFromThing :: DisassociateWirelessGatewayFromThing -> TestTree
requestDisassociateWirelessGatewayFromThing =
  req
    "DisassociateWirelessGatewayFromThing"
    "fixture/DisassociateWirelessGatewayFromThing.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateDestination :: CreateDestination -> TestTree
requestCreateDestination =
  req
    "CreateDestination"
    "fixture/CreateDestination.yaml"

requestAssociateWirelessGatewayWithCertificate :: AssociateWirelessGatewayWithCertificate -> TestTree
requestAssociateWirelessGatewayWithCertificate =
  req
    "AssociateWirelessGatewayWithCertificate"
    "fixture/AssociateWirelessGatewayWithCertificate.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdatePartnerAccount :: UpdatePartnerAccount -> TestTree
requestUpdatePartnerAccount =
  req
    "UpdatePartnerAccount"
    "fixture/UpdatePartnerAccount.yaml"

requestGetWirelessGatewayTaskDefinition :: GetWirelessGatewayTaskDefinition -> TestTree
requestGetWirelessGatewayTaskDefinition =
  req
    "GetWirelessGatewayTaskDefinition"
    "fixture/GetWirelessGatewayTaskDefinition.yaml"

requestGetResourceLogLevel :: GetResourceLogLevel -> TestTree
requestGetResourceLogLevel =
  req
    "GetResourceLogLevel"
    "fixture/GetResourceLogLevel.yaml"

requestDeleteWirelessDevice :: DeleteWirelessDevice -> TestTree
requestDeleteWirelessDevice =
  req
    "DeleteWirelessDevice"
    "fixture/DeleteWirelessDevice.yaml"

requestUpdateWirelessDevice :: UpdateWirelessDevice -> TestTree
requestUpdateWirelessDevice =
  req
    "UpdateWirelessDevice"
    "fixture/UpdateWirelessDevice.yaml"

requestListWirelessGateways :: ListWirelessGateways -> TestTree
requestListWirelessGateways =
  req
    "ListWirelessGateways"
    "fixture/ListWirelessGateways.yaml"

requestDeleteWirelessGatewayTaskDefinition :: DeleteWirelessGatewayTaskDefinition -> TestTree
requestDeleteWirelessGatewayTaskDefinition =
  req
    "DeleteWirelessGatewayTaskDefinition"
    "fixture/DeleteWirelessGatewayTaskDefinition.yaml"

requestListWirelessDevices :: ListWirelessDevices -> TestTree
requestListWirelessDevices =
  req
    "ListWirelessDevices"
    "fixture/ListWirelessDevices.yaml"

-- Responses

responseCreateWirelessGatewayTaskDefinition :: CreateWirelessGatewayTaskDefinitionResponse -> TestTree
responseCreateWirelessGatewayTaskDefinition =
  res
    "CreateWirelessGatewayTaskDefinitionResponse"
    "fixture/CreateWirelessGatewayTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWirelessGatewayTaskDefinition)

responseResetAllResourceLogLevels :: ResetAllResourceLogLevelsResponse -> TestTree
responseResetAllResourceLogLevels =
  res
    "ResetAllResourceLogLevelsResponse"
    "fixture/ResetAllResourceLogLevelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetAllResourceLogLevels)

responseListServiceProfiles :: ListServiceProfilesResponse -> TestTree
responseListServiceProfiles =
  res
    "ListServiceProfilesResponse"
    "fixture/ListServiceProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceProfiles)

responseDeleteServiceProfile :: DeleteServiceProfileResponse -> TestTree
responseDeleteServiceProfile =
  res
    "DeleteServiceProfileResponse"
    "fixture/DeleteServiceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceProfile)

responseListDeviceProfiles :: ListDeviceProfilesResponse -> TestTree
responseListDeviceProfiles =
  res
    "ListDeviceProfilesResponse"
    "fixture/ListDeviceProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceProfiles)

responseAssociateWirelessGatewayWithThing :: AssociateWirelessGatewayWithThingResponse -> TestTree
responseAssociateWirelessGatewayWithThing =
  res
    "AssociateWirelessGatewayWithThingResponse"
    "fixture/AssociateWirelessGatewayWithThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWirelessGatewayWithThing)

responseDeleteDeviceProfile :: DeleteDeviceProfileResponse -> TestTree
responseDeleteDeviceProfile =
  res
    "DeleteDeviceProfileResponse"
    "fixture/DeleteDeviceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeviceProfile)

responseAssociateWirelessDeviceWithThing :: AssociateWirelessDeviceWithThingResponse -> TestTree
responseAssociateWirelessDeviceWithThing =
  res
    "AssociateWirelessDeviceWithThingResponse"
    "fixture/AssociateWirelessDeviceWithThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWirelessDeviceWithThing)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateServiceProfile :: CreateServiceProfileResponse -> TestTree
responseCreateServiceProfile =
  res
    "CreateServiceProfileResponse"
    "fixture/CreateServiceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceProfile)

responseAssociateAwsAccountWithPartnerAccount :: AssociateAwsAccountWithPartnerAccountResponse -> TestTree
responseAssociateAwsAccountWithPartnerAccount =
  res
    "AssociateAwsAccountWithPartnerAccountResponse"
    "fixture/AssociateAwsAccountWithPartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAwsAccountWithPartnerAccount)

responseResetResourceLogLevel :: ResetResourceLogLevelResponse -> TestTree
responseResetResourceLogLevel =
  res
    "ResetResourceLogLevelResponse"
    "fixture/ResetResourceLogLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetResourceLogLevel)

responseDeleteDestination :: DeleteDestinationResponse -> TestTree
responseDeleteDestination =
  res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDestination)

responseUpdateDestination :: UpdateDestinationResponse -> TestTree
responseUpdateDestination =
  res
    "UpdateDestinationResponse"
    "fixture/UpdateDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDestination)

responseGetServiceEndpoint :: GetServiceEndpointResponse -> TestTree
responseGetServiceEndpoint =
  res
    "GetServiceEndpointResponse"
    "fixture/GetServiceEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceEndpoint)

responseGetServiceProfile :: GetServiceProfileResponse -> TestTree
responseGetServiceProfile =
  res
    "GetServiceProfileResponse"
    "fixture/GetServiceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceProfile)

responseGetWirelessDeviceStatistics :: GetWirelessDeviceStatisticsResponse -> TestTree
responseGetWirelessDeviceStatistics =
  res
    "GetWirelessDeviceStatisticsResponse"
    "fixture/GetWirelessDeviceStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessDeviceStatistics)

responseGetWirelessGatewayStatistics :: GetWirelessGatewayStatisticsResponse -> TestTree
responseGetWirelessGatewayStatistics =
  res
    "GetWirelessGatewayStatisticsResponse"
    "fixture/GetWirelessGatewayStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayStatistics)

responseGetWirelessGatewayCertificate :: GetWirelessGatewayCertificateResponse -> TestTree
responseGetWirelessGatewayCertificate =
  res
    "GetWirelessGatewayCertificateResponse"
    "fixture/GetWirelessGatewayCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayCertificate)

responseGetWirelessGatewayTask :: GetWirelessGatewayTaskResponse -> TestTree
responseGetWirelessGatewayTask =
  res
    "GetWirelessGatewayTaskResponse"
    "fixture/GetWirelessGatewayTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayTask)

responseGetWirelessGatewayFirmwareInformation :: GetWirelessGatewayFirmwareInformationResponse -> TestTree
responseGetWirelessGatewayFirmwareInformation =
  res
    "GetWirelessGatewayFirmwareInformationResponse"
    "fixture/GetWirelessGatewayFirmwareInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayFirmwareInformation)

responseListPartnerAccounts :: ListPartnerAccountsResponse -> TestTree
responseListPartnerAccounts =
  res
    "ListPartnerAccountsResponse"
    "fixture/ListPartnerAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPartnerAccounts)

responseTestWirelessDevice :: TestWirelessDeviceResponse -> TestTree
responseTestWirelessDevice =
  res
    "TestWirelessDeviceResponse"
    "fixture/TestWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestWirelessDevice)

responseGetWirelessDevice :: GetWirelessDeviceResponse -> TestTree
responseGetWirelessDevice =
  res
    "GetWirelessDeviceResponse"
    "fixture/GetWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessDevice)

responseGetWirelessGateway :: GetWirelessGatewayResponse -> TestTree
responseGetWirelessGateway =
  res
    "GetWirelessGatewayResponse"
    "fixture/GetWirelessGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGateway)

responseGetPartnerAccount :: GetPartnerAccountResponse -> TestTree
responseGetPartnerAccount =
  res
    "GetPartnerAccountResponse"
    "fixture/GetPartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPartnerAccount)

responseCreateWirelessGatewayTask :: CreateWirelessGatewayTaskResponse -> TestTree
responseCreateWirelessGatewayTask =
  res
    "CreateWirelessGatewayTaskResponse"
    "fixture/CreateWirelessGatewayTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWirelessGatewayTask)

responseDeleteWirelessGateway :: DeleteWirelessGatewayResponse -> TestTree
responseDeleteWirelessGateway =
  res
    "DeleteWirelessGatewayResponse"
    "fixture/DeleteWirelessGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWirelessGateway)

responseUpdateWirelessGateway :: UpdateWirelessGatewayResponse -> TestTree
responseUpdateWirelessGateway =
  res
    "UpdateWirelessGatewayResponse"
    "fixture/UpdateWirelessGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWirelessGateway)

responseDisassociateWirelessGatewayFromCertificate :: DisassociateWirelessGatewayFromCertificateResponse -> TestTree
responseDisassociateWirelessGatewayFromCertificate =
  res
    "DisassociateWirelessGatewayFromCertificateResponse"
    "fixture/DisassociateWirelessGatewayFromCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWirelessGatewayFromCertificate)

responseListWirelessGatewayTaskDefinitions :: ListWirelessGatewayTaskDefinitionsResponse -> TestTree
responseListWirelessGatewayTaskDefinitions =
  res
    "ListWirelessGatewayTaskDefinitionsResponse"
    "fixture/ListWirelessGatewayTaskDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWirelessGatewayTaskDefinitions)

responsePutResourceLogLevel :: PutResourceLogLevelResponse -> TestTree
responsePutResourceLogLevel =
  res
    "PutResourceLogLevelResponse"
    "fixture/PutResourceLogLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourceLogLevel)

responseCreateWirelessGateway :: CreateWirelessGatewayResponse -> TestTree
responseCreateWirelessGateway =
  res
    "CreateWirelessGatewayResponse"
    "fixture/CreateWirelessGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWirelessGateway)

responseDeleteWirelessGatewayTask :: DeleteWirelessGatewayTaskResponse -> TestTree
responseDeleteWirelessGatewayTask =
  res
    "DeleteWirelessGatewayTaskResponse"
    "fixture/DeleteWirelessGatewayTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWirelessGatewayTask)

responseCreateWirelessDevice :: CreateWirelessDeviceResponse -> TestTree
responseCreateWirelessDevice =
  res
    "CreateWirelessDeviceResponse"
    "fixture/CreateWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWirelessDevice)

responseSendDataToWirelessDevice :: SendDataToWirelessDeviceResponse -> TestTree
responseSendDataToWirelessDevice =
  res
    "SendDataToWirelessDeviceResponse"
    "fixture/SendDataToWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendDataToWirelessDevice)

responseCreateDeviceProfile :: CreateDeviceProfileResponse -> TestTree
responseCreateDeviceProfile =
  res
    "CreateDeviceProfileResponse"
    "fixture/CreateDeviceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeviceProfile)

responseDisassociateAwsAccountFromPartnerAccount :: DisassociateAwsAccountFromPartnerAccountResponse -> TestTree
responseDisassociateAwsAccountFromPartnerAccount =
  res
    "DisassociateAwsAccountFromPartnerAccountResponse"
    "fixture/DisassociateAwsAccountFromPartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAwsAccountFromPartnerAccount)

responseUpdateLogLevelsByResourceTypes :: UpdateLogLevelsByResourceTypesResponse -> TestTree
responseUpdateLogLevelsByResourceTypes =
  res
    "UpdateLogLevelsByResourceTypesResponse"
    "fixture/UpdateLogLevelsByResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLogLevelsByResourceTypes)

responseGetDestination :: GetDestinationResponse -> TestTree
responseGetDestination =
  res
    "GetDestinationResponse"
    "fixture/GetDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDestination)

responseGetLogLevelsByResourceTypes :: GetLogLevelsByResourceTypesResponse -> TestTree
responseGetLogLevelsByResourceTypes =
  res
    "GetLogLevelsByResourceTypesResponse"
    "fixture/GetLogLevelsByResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLogLevelsByResourceTypes)

responseListDestinations :: ListDestinationsResponse -> TestTree
responseListDestinations =
  res
    "ListDestinationsResponse"
    "fixture/ListDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDestinations)

responseGetDeviceProfile :: GetDeviceProfileResponse -> TestTree
responseGetDeviceProfile =
  res
    "GetDeviceProfileResponse"
    "fixture/GetDeviceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceProfile)

responseDisassociateWirelessDeviceFromThing :: DisassociateWirelessDeviceFromThingResponse -> TestTree
responseDisassociateWirelessDeviceFromThing =
  res
    "DisassociateWirelessDeviceFromThingResponse"
    "fixture/DisassociateWirelessDeviceFromThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWirelessDeviceFromThing)

responseDisassociateWirelessGatewayFromThing :: DisassociateWirelessGatewayFromThingResponse -> TestTree
responseDisassociateWirelessGatewayFromThing =
  res
    "DisassociateWirelessGatewayFromThingResponse"
    "fixture/DisassociateWirelessGatewayFromThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWirelessGatewayFromThing)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateDestination :: CreateDestinationResponse -> TestTree
responseCreateDestination =
  res
    "CreateDestinationResponse"
    "fixture/CreateDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDestination)

responseAssociateWirelessGatewayWithCertificate :: AssociateWirelessGatewayWithCertificateResponse -> TestTree
responseAssociateWirelessGatewayWithCertificate =
  res
    "AssociateWirelessGatewayWithCertificateResponse"
    "fixture/AssociateWirelessGatewayWithCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWirelessGatewayWithCertificate)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdatePartnerAccount :: UpdatePartnerAccountResponse -> TestTree
responseUpdatePartnerAccount =
  res
    "UpdatePartnerAccountResponse"
    "fixture/UpdatePartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePartnerAccount)

responseGetWirelessGatewayTaskDefinition :: GetWirelessGatewayTaskDefinitionResponse -> TestTree
responseGetWirelessGatewayTaskDefinition =
  res
    "GetWirelessGatewayTaskDefinitionResponse"
    "fixture/GetWirelessGatewayTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayTaskDefinition)

responseGetResourceLogLevel :: GetResourceLogLevelResponse -> TestTree
responseGetResourceLogLevel =
  res
    "GetResourceLogLevelResponse"
    "fixture/GetResourceLogLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceLogLevel)

responseDeleteWirelessDevice :: DeleteWirelessDeviceResponse -> TestTree
responseDeleteWirelessDevice =
  res
    "DeleteWirelessDeviceResponse"
    "fixture/DeleteWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWirelessDevice)

responseUpdateWirelessDevice :: UpdateWirelessDeviceResponse -> TestTree
responseUpdateWirelessDevice =
  res
    "UpdateWirelessDeviceResponse"
    "fixture/UpdateWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWirelessDevice)

responseListWirelessGateways :: ListWirelessGatewaysResponse -> TestTree
responseListWirelessGateways =
  res
    "ListWirelessGatewaysResponse"
    "fixture/ListWirelessGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWirelessGateways)

responseDeleteWirelessGatewayTaskDefinition :: DeleteWirelessGatewayTaskDefinitionResponse -> TestTree
responseDeleteWirelessGatewayTaskDefinition =
  res
    "DeleteWirelessGatewayTaskDefinitionResponse"
    "fixture/DeleteWirelessGatewayTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWirelessGatewayTaskDefinition)

responseListWirelessDevices :: ListWirelessDevicesResponse -> TestTree
responseListWirelessDevices =
  res
    "ListWirelessDevicesResponse"
    "fixture/ListWirelessDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWirelessDevices)
