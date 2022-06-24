{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTWireless
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTWireless where

import Amazonka.IoTWireless
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTWireless.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateAwsAccountWithPartnerAccount $
--             newAssociateAwsAccountWithPartnerAccount
--
--         , requestAssociateWirelessDeviceWithThing $
--             newAssociateWirelessDeviceWithThing
--
--         , requestAssociateWirelessGatewayWithCertificate $
--             newAssociateWirelessGatewayWithCertificate
--
--         , requestAssociateWirelessGatewayWithThing $
--             newAssociateWirelessGatewayWithThing
--
--         , requestCreateDestination $
--             newCreateDestination
--
--         , requestCreateDeviceProfile $
--             newCreateDeviceProfile
--
--         , requestCreateServiceProfile $
--             newCreateServiceProfile
--
--         , requestCreateWirelessDevice $
--             newCreateWirelessDevice
--
--         , requestCreateWirelessGateway $
--             newCreateWirelessGateway
--
--         , requestCreateWirelessGatewayTask $
--             newCreateWirelessGatewayTask
--
--         , requestCreateWirelessGatewayTaskDefinition $
--             newCreateWirelessGatewayTaskDefinition
--
--         , requestDeleteDestination $
--             newDeleteDestination
--
--         , requestDeleteDeviceProfile $
--             newDeleteDeviceProfile
--
--         , requestDeleteServiceProfile $
--             newDeleteServiceProfile
--
--         , requestDeleteWirelessDevice $
--             newDeleteWirelessDevice
--
--         , requestDeleteWirelessGateway $
--             newDeleteWirelessGateway
--
--         , requestDeleteWirelessGatewayTask $
--             newDeleteWirelessGatewayTask
--
--         , requestDeleteWirelessGatewayTaskDefinition $
--             newDeleteWirelessGatewayTaskDefinition
--
--         , requestDisassociateAwsAccountFromPartnerAccount $
--             newDisassociateAwsAccountFromPartnerAccount
--
--         , requestDisassociateWirelessDeviceFromThing $
--             newDisassociateWirelessDeviceFromThing
--
--         , requestDisassociateWirelessGatewayFromCertificate $
--             newDisassociateWirelessGatewayFromCertificate
--
--         , requestDisassociateWirelessGatewayFromThing $
--             newDisassociateWirelessGatewayFromThing
--
--         , requestGetDestination $
--             newGetDestination
--
--         , requestGetDeviceProfile $
--             newGetDeviceProfile
--
--         , requestGetLogLevelsByResourceTypes $
--             newGetLogLevelsByResourceTypes
--
--         , requestGetPartnerAccount $
--             newGetPartnerAccount
--
--         , requestGetResourceLogLevel $
--             newGetResourceLogLevel
--
--         , requestGetServiceEndpoint $
--             newGetServiceEndpoint
--
--         , requestGetServiceProfile $
--             newGetServiceProfile
--
--         , requestGetWirelessDevice $
--             newGetWirelessDevice
--
--         , requestGetWirelessDeviceStatistics $
--             newGetWirelessDeviceStatistics
--
--         , requestGetWirelessGateway $
--             newGetWirelessGateway
--
--         , requestGetWirelessGatewayCertificate $
--             newGetWirelessGatewayCertificate
--
--         , requestGetWirelessGatewayFirmwareInformation $
--             newGetWirelessGatewayFirmwareInformation
--
--         , requestGetWirelessGatewayStatistics $
--             newGetWirelessGatewayStatistics
--
--         , requestGetWirelessGatewayTask $
--             newGetWirelessGatewayTask
--
--         , requestGetWirelessGatewayTaskDefinition $
--             newGetWirelessGatewayTaskDefinition
--
--         , requestListDestinations $
--             newListDestinations
--
--         , requestListDeviceProfiles $
--             newListDeviceProfiles
--
--         , requestListPartnerAccounts $
--             newListPartnerAccounts
--
--         , requestListServiceProfiles $
--             newListServiceProfiles
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWirelessDevices $
--             newListWirelessDevices
--
--         , requestListWirelessGatewayTaskDefinitions $
--             newListWirelessGatewayTaskDefinitions
--
--         , requestListWirelessGateways $
--             newListWirelessGateways
--
--         , requestPutResourceLogLevel $
--             newPutResourceLogLevel
--
--         , requestResetAllResourceLogLevels $
--             newResetAllResourceLogLevels
--
--         , requestResetResourceLogLevel $
--             newResetResourceLogLevel
--
--         , requestSendDataToWirelessDevice $
--             newSendDataToWirelessDevice
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestWirelessDevice $
--             newTestWirelessDevice
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDestination $
--             newUpdateDestination
--
--         , requestUpdateLogLevelsByResourceTypes $
--             newUpdateLogLevelsByResourceTypes
--
--         , requestUpdatePartnerAccount $
--             newUpdatePartnerAccount
--
--         , requestUpdateWirelessDevice $
--             newUpdateWirelessDevice
--
--         , requestUpdateWirelessGateway $
--             newUpdateWirelessGateway
--
--           ]

--     , testGroup "response"
--         [ responseAssociateAwsAccountWithPartnerAccount $
--             newAssociateAwsAccountWithPartnerAccountResponse
--
--         , responseAssociateWirelessDeviceWithThing $
--             newAssociateWirelessDeviceWithThingResponse
--
--         , responseAssociateWirelessGatewayWithCertificate $
--             newAssociateWirelessGatewayWithCertificateResponse
--
--         , responseAssociateWirelessGatewayWithThing $
--             newAssociateWirelessGatewayWithThingResponse
--
--         , responseCreateDestination $
--             newCreateDestinationResponse
--
--         , responseCreateDeviceProfile $
--             newCreateDeviceProfileResponse
--
--         , responseCreateServiceProfile $
--             newCreateServiceProfileResponse
--
--         , responseCreateWirelessDevice $
--             newCreateWirelessDeviceResponse
--
--         , responseCreateWirelessGateway $
--             newCreateWirelessGatewayResponse
--
--         , responseCreateWirelessGatewayTask $
--             newCreateWirelessGatewayTaskResponse
--
--         , responseCreateWirelessGatewayTaskDefinition $
--             newCreateWirelessGatewayTaskDefinitionResponse
--
--         , responseDeleteDestination $
--             newDeleteDestinationResponse
--
--         , responseDeleteDeviceProfile $
--             newDeleteDeviceProfileResponse
--
--         , responseDeleteServiceProfile $
--             newDeleteServiceProfileResponse
--
--         , responseDeleteWirelessDevice $
--             newDeleteWirelessDeviceResponse
--
--         , responseDeleteWirelessGateway $
--             newDeleteWirelessGatewayResponse
--
--         , responseDeleteWirelessGatewayTask $
--             newDeleteWirelessGatewayTaskResponse
--
--         , responseDeleteWirelessGatewayTaskDefinition $
--             newDeleteWirelessGatewayTaskDefinitionResponse
--
--         , responseDisassociateAwsAccountFromPartnerAccount $
--             newDisassociateAwsAccountFromPartnerAccountResponse
--
--         , responseDisassociateWirelessDeviceFromThing $
--             newDisassociateWirelessDeviceFromThingResponse
--
--         , responseDisassociateWirelessGatewayFromCertificate $
--             newDisassociateWirelessGatewayFromCertificateResponse
--
--         , responseDisassociateWirelessGatewayFromThing $
--             newDisassociateWirelessGatewayFromThingResponse
--
--         , responseGetDestination $
--             newGetDestinationResponse
--
--         , responseGetDeviceProfile $
--             newGetDeviceProfileResponse
--
--         , responseGetLogLevelsByResourceTypes $
--             newGetLogLevelsByResourceTypesResponse
--
--         , responseGetPartnerAccount $
--             newGetPartnerAccountResponse
--
--         , responseGetResourceLogLevel $
--             newGetResourceLogLevelResponse
--
--         , responseGetServiceEndpoint $
--             newGetServiceEndpointResponse
--
--         , responseGetServiceProfile $
--             newGetServiceProfileResponse
--
--         , responseGetWirelessDevice $
--             newGetWirelessDeviceResponse
--
--         , responseGetWirelessDeviceStatistics $
--             newGetWirelessDeviceStatisticsResponse
--
--         , responseGetWirelessGateway $
--             newGetWirelessGatewayResponse
--
--         , responseGetWirelessGatewayCertificate $
--             newGetWirelessGatewayCertificateResponse
--
--         , responseGetWirelessGatewayFirmwareInformation $
--             newGetWirelessGatewayFirmwareInformationResponse
--
--         , responseGetWirelessGatewayStatistics $
--             newGetWirelessGatewayStatisticsResponse
--
--         , responseGetWirelessGatewayTask $
--             newGetWirelessGatewayTaskResponse
--
--         , responseGetWirelessGatewayTaskDefinition $
--             newGetWirelessGatewayTaskDefinitionResponse
--
--         , responseListDestinations $
--             newListDestinationsResponse
--
--         , responseListDeviceProfiles $
--             newListDeviceProfilesResponse
--
--         , responseListPartnerAccounts $
--             newListPartnerAccountsResponse
--
--         , responseListServiceProfiles $
--             newListServiceProfilesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWirelessDevices $
--             newListWirelessDevicesResponse
--
--         , responseListWirelessGatewayTaskDefinitions $
--             newListWirelessGatewayTaskDefinitionsResponse
--
--         , responseListWirelessGateways $
--             newListWirelessGatewaysResponse
--
--         , responsePutResourceLogLevel $
--             newPutResourceLogLevelResponse
--
--         , responseResetAllResourceLogLevels $
--             newResetAllResourceLogLevelsResponse
--
--         , responseResetResourceLogLevel $
--             newResetResourceLogLevelResponse
--
--         , responseSendDataToWirelessDevice $
--             newSendDataToWirelessDeviceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestWirelessDevice $
--             newTestWirelessDeviceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDestination $
--             newUpdateDestinationResponse
--
--         , responseUpdateLogLevelsByResourceTypes $
--             newUpdateLogLevelsByResourceTypesResponse
--
--         , responseUpdatePartnerAccount $
--             newUpdatePartnerAccountResponse
--
--         , responseUpdateWirelessDevice $
--             newUpdateWirelessDeviceResponse
--
--         , responseUpdateWirelessGateway $
--             newUpdateWirelessGatewayResponse
--
--           ]
--     ]

-- Requests

requestAssociateAwsAccountWithPartnerAccount :: AssociateAwsAccountWithPartnerAccount -> TestTree
requestAssociateAwsAccountWithPartnerAccount =
  req
    "AssociateAwsAccountWithPartnerAccount"
    "fixture/AssociateAwsAccountWithPartnerAccount.yaml"

requestAssociateWirelessDeviceWithThing :: AssociateWirelessDeviceWithThing -> TestTree
requestAssociateWirelessDeviceWithThing =
  req
    "AssociateWirelessDeviceWithThing"
    "fixture/AssociateWirelessDeviceWithThing.yaml"

requestAssociateWirelessGatewayWithCertificate :: AssociateWirelessGatewayWithCertificate -> TestTree
requestAssociateWirelessGatewayWithCertificate =
  req
    "AssociateWirelessGatewayWithCertificate"
    "fixture/AssociateWirelessGatewayWithCertificate.yaml"

requestAssociateWirelessGatewayWithThing :: AssociateWirelessGatewayWithThing -> TestTree
requestAssociateWirelessGatewayWithThing =
  req
    "AssociateWirelessGatewayWithThing"
    "fixture/AssociateWirelessGatewayWithThing.yaml"

requestCreateDestination :: CreateDestination -> TestTree
requestCreateDestination =
  req
    "CreateDestination"
    "fixture/CreateDestination.yaml"

requestCreateDeviceProfile :: CreateDeviceProfile -> TestTree
requestCreateDeviceProfile =
  req
    "CreateDeviceProfile"
    "fixture/CreateDeviceProfile.yaml"

requestCreateServiceProfile :: CreateServiceProfile -> TestTree
requestCreateServiceProfile =
  req
    "CreateServiceProfile"
    "fixture/CreateServiceProfile.yaml"

requestCreateWirelessDevice :: CreateWirelessDevice -> TestTree
requestCreateWirelessDevice =
  req
    "CreateWirelessDevice"
    "fixture/CreateWirelessDevice.yaml"

requestCreateWirelessGateway :: CreateWirelessGateway -> TestTree
requestCreateWirelessGateway =
  req
    "CreateWirelessGateway"
    "fixture/CreateWirelessGateway.yaml"

requestCreateWirelessGatewayTask :: CreateWirelessGatewayTask -> TestTree
requestCreateWirelessGatewayTask =
  req
    "CreateWirelessGatewayTask"
    "fixture/CreateWirelessGatewayTask.yaml"

requestCreateWirelessGatewayTaskDefinition :: CreateWirelessGatewayTaskDefinition -> TestTree
requestCreateWirelessGatewayTaskDefinition =
  req
    "CreateWirelessGatewayTaskDefinition"
    "fixture/CreateWirelessGatewayTaskDefinition.yaml"

requestDeleteDestination :: DeleteDestination -> TestTree
requestDeleteDestination =
  req
    "DeleteDestination"
    "fixture/DeleteDestination.yaml"

requestDeleteDeviceProfile :: DeleteDeviceProfile -> TestTree
requestDeleteDeviceProfile =
  req
    "DeleteDeviceProfile"
    "fixture/DeleteDeviceProfile.yaml"

requestDeleteServiceProfile :: DeleteServiceProfile -> TestTree
requestDeleteServiceProfile =
  req
    "DeleteServiceProfile"
    "fixture/DeleteServiceProfile.yaml"

requestDeleteWirelessDevice :: DeleteWirelessDevice -> TestTree
requestDeleteWirelessDevice =
  req
    "DeleteWirelessDevice"
    "fixture/DeleteWirelessDevice.yaml"

requestDeleteWirelessGateway :: DeleteWirelessGateway -> TestTree
requestDeleteWirelessGateway =
  req
    "DeleteWirelessGateway"
    "fixture/DeleteWirelessGateway.yaml"

requestDeleteWirelessGatewayTask :: DeleteWirelessGatewayTask -> TestTree
requestDeleteWirelessGatewayTask =
  req
    "DeleteWirelessGatewayTask"
    "fixture/DeleteWirelessGatewayTask.yaml"

requestDeleteWirelessGatewayTaskDefinition :: DeleteWirelessGatewayTaskDefinition -> TestTree
requestDeleteWirelessGatewayTaskDefinition =
  req
    "DeleteWirelessGatewayTaskDefinition"
    "fixture/DeleteWirelessGatewayTaskDefinition.yaml"

requestDisassociateAwsAccountFromPartnerAccount :: DisassociateAwsAccountFromPartnerAccount -> TestTree
requestDisassociateAwsAccountFromPartnerAccount =
  req
    "DisassociateAwsAccountFromPartnerAccount"
    "fixture/DisassociateAwsAccountFromPartnerAccount.yaml"

requestDisassociateWirelessDeviceFromThing :: DisassociateWirelessDeviceFromThing -> TestTree
requestDisassociateWirelessDeviceFromThing =
  req
    "DisassociateWirelessDeviceFromThing"
    "fixture/DisassociateWirelessDeviceFromThing.yaml"

requestDisassociateWirelessGatewayFromCertificate :: DisassociateWirelessGatewayFromCertificate -> TestTree
requestDisassociateWirelessGatewayFromCertificate =
  req
    "DisassociateWirelessGatewayFromCertificate"
    "fixture/DisassociateWirelessGatewayFromCertificate.yaml"

requestDisassociateWirelessGatewayFromThing :: DisassociateWirelessGatewayFromThing -> TestTree
requestDisassociateWirelessGatewayFromThing =
  req
    "DisassociateWirelessGatewayFromThing"
    "fixture/DisassociateWirelessGatewayFromThing.yaml"

requestGetDestination :: GetDestination -> TestTree
requestGetDestination =
  req
    "GetDestination"
    "fixture/GetDestination.yaml"

requestGetDeviceProfile :: GetDeviceProfile -> TestTree
requestGetDeviceProfile =
  req
    "GetDeviceProfile"
    "fixture/GetDeviceProfile.yaml"

requestGetLogLevelsByResourceTypes :: GetLogLevelsByResourceTypes -> TestTree
requestGetLogLevelsByResourceTypes =
  req
    "GetLogLevelsByResourceTypes"
    "fixture/GetLogLevelsByResourceTypes.yaml"

requestGetPartnerAccount :: GetPartnerAccount -> TestTree
requestGetPartnerAccount =
  req
    "GetPartnerAccount"
    "fixture/GetPartnerAccount.yaml"

requestGetResourceLogLevel :: GetResourceLogLevel -> TestTree
requestGetResourceLogLevel =
  req
    "GetResourceLogLevel"
    "fixture/GetResourceLogLevel.yaml"

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

requestGetWirelessDevice :: GetWirelessDevice -> TestTree
requestGetWirelessDevice =
  req
    "GetWirelessDevice"
    "fixture/GetWirelessDevice.yaml"

requestGetWirelessDeviceStatistics :: GetWirelessDeviceStatistics -> TestTree
requestGetWirelessDeviceStatistics =
  req
    "GetWirelessDeviceStatistics"
    "fixture/GetWirelessDeviceStatistics.yaml"

requestGetWirelessGateway :: GetWirelessGateway -> TestTree
requestGetWirelessGateway =
  req
    "GetWirelessGateway"
    "fixture/GetWirelessGateway.yaml"

requestGetWirelessGatewayCertificate :: GetWirelessGatewayCertificate -> TestTree
requestGetWirelessGatewayCertificate =
  req
    "GetWirelessGatewayCertificate"
    "fixture/GetWirelessGatewayCertificate.yaml"

requestGetWirelessGatewayFirmwareInformation :: GetWirelessGatewayFirmwareInformation -> TestTree
requestGetWirelessGatewayFirmwareInformation =
  req
    "GetWirelessGatewayFirmwareInformation"
    "fixture/GetWirelessGatewayFirmwareInformation.yaml"

requestGetWirelessGatewayStatistics :: GetWirelessGatewayStatistics -> TestTree
requestGetWirelessGatewayStatistics =
  req
    "GetWirelessGatewayStatistics"
    "fixture/GetWirelessGatewayStatistics.yaml"

requestGetWirelessGatewayTask :: GetWirelessGatewayTask -> TestTree
requestGetWirelessGatewayTask =
  req
    "GetWirelessGatewayTask"
    "fixture/GetWirelessGatewayTask.yaml"

requestGetWirelessGatewayTaskDefinition :: GetWirelessGatewayTaskDefinition -> TestTree
requestGetWirelessGatewayTaskDefinition =
  req
    "GetWirelessGatewayTaskDefinition"
    "fixture/GetWirelessGatewayTaskDefinition.yaml"

requestListDestinations :: ListDestinations -> TestTree
requestListDestinations =
  req
    "ListDestinations"
    "fixture/ListDestinations.yaml"

requestListDeviceProfiles :: ListDeviceProfiles -> TestTree
requestListDeviceProfiles =
  req
    "ListDeviceProfiles"
    "fixture/ListDeviceProfiles.yaml"

requestListPartnerAccounts :: ListPartnerAccounts -> TestTree
requestListPartnerAccounts =
  req
    "ListPartnerAccounts"
    "fixture/ListPartnerAccounts.yaml"

requestListServiceProfiles :: ListServiceProfiles -> TestTree
requestListServiceProfiles =
  req
    "ListServiceProfiles"
    "fixture/ListServiceProfiles.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWirelessDevices :: ListWirelessDevices -> TestTree
requestListWirelessDevices =
  req
    "ListWirelessDevices"
    "fixture/ListWirelessDevices.yaml"

requestListWirelessGatewayTaskDefinitions :: ListWirelessGatewayTaskDefinitions -> TestTree
requestListWirelessGatewayTaskDefinitions =
  req
    "ListWirelessGatewayTaskDefinitions"
    "fixture/ListWirelessGatewayTaskDefinitions.yaml"

requestListWirelessGateways :: ListWirelessGateways -> TestTree
requestListWirelessGateways =
  req
    "ListWirelessGateways"
    "fixture/ListWirelessGateways.yaml"

requestPutResourceLogLevel :: PutResourceLogLevel -> TestTree
requestPutResourceLogLevel =
  req
    "PutResourceLogLevel"
    "fixture/PutResourceLogLevel.yaml"

requestResetAllResourceLogLevels :: ResetAllResourceLogLevels -> TestTree
requestResetAllResourceLogLevels =
  req
    "ResetAllResourceLogLevels"
    "fixture/ResetAllResourceLogLevels.yaml"

requestResetResourceLogLevel :: ResetResourceLogLevel -> TestTree
requestResetResourceLogLevel =
  req
    "ResetResourceLogLevel"
    "fixture/ResetResourceLogLevel.yaml"

requestSendDataToWirelessDevice :: SendDataToWirelessDevice -> TestTree
requestSendDataToWirelessDevice =
  req
    "SendDataToWirelessDevice"
    "fixture/SendDataToWirelessDevice.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestWirelessDevice :: TestWirelessDevice -> TestTree
requestTestWirelessDevice =
  req
    "TestWirelessDevice"
    "fixture/TestWirelessDevice.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateDestination :: UpdateDestination -> TestTree
requestUpdateDestination =
  req
    "UpdateDestination"
    "fixture/UpdateDestination.yaml"

requestUpdateLogLevelsByResourceTypes :: UpdateLogLevelsByResourceTypes -> TestTree
requestUpdateLogLevelsByResourceTypes =
  req
    "UpdateLogLevelsByResourceTypes"
    "fixture/UpdateLogLevelsByResourceTypes.yaml"

requestUpdatePartnerAccount :: UpdatePartnerAccount -> TestTree
requestUpdatePartnerAccount =
  req
    "UpdatePartnerAccount"
    "fixture/UpdatePartnerAccount.yaml"

requestUpdateWirelessDevice :: UpdateWirelessDevice -> TestTree
requestUpdateWirelessDevice =
  req
    "UpdateWirelessDevice"
    "fixture/UpdateWirelessDevice.yaml"

requestUpdateWirelessGateway :: UpdateWirelessGateway -> TestTree
requestUpdateWirelessGateway =
  req
    "UpdateWirelessGateway"
    "fixture/UpdateWirelessGateway.yaml"

-- Responses

responseAssociateAwsAccountWithPartnerAccount :: AssociateAwsAccountWithPartnerAccountResponse -> TestTree
responseAssociateAwsAccountWithPartnerAccount =
  res
    "AssociateAwsAccountWithPartnerAccountResponse"
    "fixture/AssociateAwsAccountWithPartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAwsAccountWithPartnerAccount)

responseAssociateWirelessDeviceWithThing :: AssociateWirelessDeviceWithThingResponse -> TestTree
responseAssociateWirelessDeviceWithThing =
  res
    "AssociateWirelessDeviceWithThingResponse"
    "fixture/AssociateWirelessDeviceWithThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWirelessDeviceWithThing)

responseAssociateWirelessGatewayWithCertificate :: AssociateWirelessGatewayWithCertificateResponse -> TestTree
responseAssociateWirelessGatewayWithCertificate =
  res
    "AssociateWirelessGatewayWithCertificateResponse"
    "fixture/AssociateWirelessGatewayWithCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWirelessGatewayWithCertificate)

responseAssociateWirelessGatewayWithThing :: AssociateWirelessGatewayWithThingResponse -> TestTree
responseAssociateWirelessGatewayWithThing =
  res
    "AssociateWirelessGatewayWithThingResponse"
    "fixture/AssociateWirelessGatewayWithThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWirelessGatewayWithThing)

responseCreateDestination :: CreateDestinationResponse -> TestTree
responseCreateDestination =
  res
    "CreateDestinationResponse"
    "fixture/CreateDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDestination)

responseCreateDeviceProfile :: CreateDeviceProfileResponse -> TestTree
responseCreateDeviceProfile =
  res
    "CreateDeviceProfileResponse"
    "fixture/CreateDeviceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeviceProfile)

responseCreateServiceProfile :: CreateServiceProfileResponse -> TestTree
responseCreateServiceProfile =
  res
    "CreateServiceProfileResponse"
    "fixture/CreateServiceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceProfile)

responseCreateWirelessDevice :: CreateWirelessDeviceResponse -> TestTree
responseCreateWirelessDevice =
  res
    "CreateWirelessDeviceResponse"
    "fixture/CreateWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWirelessDevice)

responseCreateWirelessGateway :: CreateWirelessGatewayResponse -> TestTree
responseCreateWirelessGateway =
  res
    "CreateWirelessGatewayResponse"
    "fixture/CreateWirelessGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWirelessGateway)

responseCreateWirelessGatewayTask :: CreateWirelessGatewayTaskResponse -> TestTree
responseCreateWirelessGatewayTask =
  res
    "CreateWirelessGatewayTaskResponse"
    "fixture/CreateWirelessGatewayTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWirelessGatewayTask)

responseCreateWirelessGatewayTaskDefinition :: CreateWirelessGatewayTaskDefinitionResponse -> TestTree
responseCreateWirelessGatewayTaskDefinition =
  res
    "CreateWirelessGatewayTaskDefinitionResponse"
    "fixture/CreateWirelessGatewayTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWirelessGatewayTaskDefinition)

responseDeleteDestination :: DeleteDestinationResponse -> TestTree
responseDeleteDestination =
  res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDestination)

responseDeleteDeviceProfile :: DeleteDeviceProfileResponse -> TestTree
responseDeleteDeviceProfile =
  res
    "DeleteDeviceProfileResponse"
    "fixture/DeleteDeviceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeviceProfile)

responseDeleteServiceProfile :: DeleteServiceProfileResponse -> TestTree
responseDeleteServiceProfile =
  res
    "DeleteServiceProfileResponse"
    "fixture/DeleteServiceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceProfile)

responseDeleteWirelessDevice :: DeleteWirelessDeviceResponse -> TestTree
responseDeleteWirelessDevice =
  res
    "DeleteWirelessDeviceResponse"
    "fixture/DeleteWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWirelessDevice)

responseDeleteWirelessGateway :: DeleteWirelessGatewayResponse -> TestTree
responseDeleteWirelessGateway =
  res
    "DeleteWirelessGatewayResponse"
    "fixture/DeleteWirelessGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWirelessGateway)

responseDeleteWirelessGatewayTask :: DeleteWirelessGatewayTaskResponse -> TestTree
responseDeleteWirelessGatewayTask =
  res
    "DeleteWirelessGatewayTaskResponse"
    "fixture/DeleteWirelessGatewayTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWirelessGatewayTask)

responseDeleteWirelessGatewayTaskDefinition :: DeleteWirelessGatewayTaskDefinitionResponse -> TestTree
responseDeleteWirelessGatewayTaskDefinition =
  res
    "DeleteWirelessGatewayTaskDefinitionResponse"
    "fixture/DeleteWirelessGatewayTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWirelessGatewayTaskDefinition)

responseDisassociateAwsAccountFromPartnerAccount :: DisassociateAwsAccountFromPartnerAccountResponse -> TestTree
responseDisassociateAwsAccountFromPartnerAccount =
  res
    "DisassociateAwsAccountFromPartnerAccountResponse"
    "fixture/DisassociateAwsAccountFromPartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAwsAccountFromPartnerAccount)

responseDisassociateWirelessDeviceFromThing :: DisassociateWirelessDeviceFromThingResponse -> TestTree
responseDisassociateWirelessDeviceFromThing =
  res
    "DisassociateWirelessDeviceFromThingResponse"
    "fixture/DisassociateWirelessDeviceFromThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWirelessDeviceFromThing)

responseDisassociateWirelessGatewayFromCertificate :: DisassociateWirelessGatewayFromCertificateResponse -> TestTree
responseDisassociateWirelessGatewayFromCertificate =
  res
    "DisassociateWirelessGatewayFromCertificateResponse"
    "fixture/DisassociateWirelessGatewayFromCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWirelessGatewayFromCertificate)

responseDisassociateWirelessGatewayFromThing :: DisassociateWirelessGatewayFromThingResponse -> TestTree
responseDisassociateWirelessGatewayFromThing =
  res
    "DisassociateWirelessGatewayFromThingResponse"
    "fixture/DisassociateWirelessGatewayFromThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWirelessGatewayFromThing)

responseGetDestination :: GetDestinationResponse -> TestTree
responseGetDestination =
  res
    "GetDestinationResponse"
    "fixture/GetDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDestination)

responseGetDeviceProfile :: GetDeviceProfileResponse -> TestTree
responseGetDeviceProfile =
  res
    "GetDeviceProfileResponse"
    "fixture/GetDeviceProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceProfile)

responseGetLogLevelsByResourceTypes :: GetLogLevelsByResourceTypesResponse -> TestTree
responseGetLogLevelsByResourceTypes =
  res
    "GetLogLevelsByResourceTypesResponse"
    "fixture/GetLogLevelsByResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLogLevelsByResourceTypes)

responseGetPartnerAccount :: GetPartnerAccountResponse -> TestTree
responseGetPartnerAccount =
  res
    "GetPartnerAccountResponse"
    "fixture/GetPartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPartnerAccount)

responseGetResourceLogLevel :: GetResourceLogLevelResponse -> TestTree
responseGetResourceLogLevel =
  res
    "GetResourceLogLevelResponse"
    "fixture/GetResourceLogLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceLogLevel)

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

responseGetWirelessDevice :: GetWirelessDeviceResponse -> TestTree
responseGetWirelessDevice =
  res
    "GetWirelessDeviceResponse"
    "fixture/GetWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessDevice)

responseGetWirelessDeviceStatistics :: GetWirelessDeviceStatisticsResponse -> TestTree
responseGetWirelessDeviceStatistics =
  res
    "GetWirelessDeviceStatisticsResponse"
    "fixture/GetWirelessDeviceStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessDeviceStatistics)

responseGetWirelessGateway :: GetWirelessGatewayResponse -> TestTree
responseGetWirelessGateway =
  res
    "GetWirelessGatewayResponse"
    "fixture/GetWirelessGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGateway)

responseGetWirelessGatewayCertificate :: GetWirelessGatewayCertificateResponse -> TestTree
responseGetWirelessGatewayCertificate =
  res
    "GetWirelessGatewayCertificateResponse"
    "fixture/GetWirelessGatewayCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayCertificate)

responseGetWirelessGatewayFirmwareInformation :: GetWirelessGatewayFirmwareInformationResponse -> TestTree
responseGetWirelessGatewayFirmwareInformation =
  res
    "GetWirelessGatewayFirmwareInformationResponse"
    "fixture/GetWirelessGatewayFirmwareInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayFirmwareInformation)

responseGetWirelessGatewayStatistics :: GetWirelessGatewayStatisticsResponse -> TestTree
responseGetWirelessGatewayStatistics =
  res
    "GetWirelessGatewayStatisticsResponse"
    "fixture/GetWirelessGatewayStatisticsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayStatistics)

responseGetWirelessGatewayTask :: GetWirelessGatewayTaskResponse -> TestTree
responseGetWirelessGatewayTask =
  res
    "GetWirelessGatewayTaskResponse"
    "fixture/GetWirelessGatewayTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayTask)

responseGetWirelessGatewayTaskDefinition :: GetWirelessGatewayTaskDefinitionResponse -> TestTree
responseGetWirelessGatewayTaskDefinition =
  res
    "GetWirelessGatewayTaskDefinitionResponse"
    "fixture/GetWirelessGatewayTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWirelessGatewayTaskDefinition)

responseListDestinations :: ListDestinationsResponse -> TestTree
responseListDestinations =
  res
    "ListDestinationsResponse"
    "fixture/ListDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDestinations)

responseListDeviceProfiles :: ListDeviceProfilesResponse -> TestTree
responseListDeviceProfiles =
  res
    "ListDeviceProfilesResponse"
    "fixture/ListDeviceProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceProfiles)

responseListPartnerAccounts :: ListPartnerAccountsResponse -> TestTree
responseListPartnerAccounts =
  res
    "ListPartnerAccountsResponse"
    "fixture/ListPartnerAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPartnerAccounts)

responseListServiceProfiles :: ListServiceProfilesResponse -> TestTree
responseListServiceProfiles =
  res
    "ListServiceProfilesResponse"
    "fixture/ListServiceProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceProfiles)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWirelessDevices :: ListWirelessDevicesResponse -> TestTree
responseListWirelessDevices =
  res
    "ListWirelessDevicesResponse"
    "fixture/ListWirelessDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWirelessDevices)

responseListWirelessGatewayTaskDefinitions :: ListWirelessGatewayTaskDefinitionsResponse -> TestTree
responseListWirelessGatewayTaskDefinitions =
  res
    "ListWirelessGatewayTaskDefinitionsResponse"
    "fixture/ListWirelessGatewayTaskDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWirelessGatewayTaskDefinitions)

responseListWirelessGateways :: ListWirelessGatewaysResponse -> TestTree
responseListWirelessGateways =
  res
    "ListWirelessGatewaysResponse"
    "fixture/ListWirelessGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWirelessGateways)

responsePutResourceLogLevel :: PutResourceLogLevelResponse -> TestTree
responsePutResourceLogLevel =
  res
    "PutResourceLogLevelResponse"
    "fixture/PutResourceLogLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourceLogLevel)

responseResetAllResourceLogLevels :: ResetAllResourceLogLevelsResponse -> TestTree
responseResetAllResourceLogLevels =
  res
    "ResetAllResourceLogLevelsResponse"
    "fixture/ResetAllResourceLogLevelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetAllResourceLogLevels)

responseResetResourceLogLevel :: ResetResourceLogLevelResponse -> TestTree
responseResetResourceLogLevel =
  res
    "ResetResourceLogLevelResponse"
    "fixture/ResetResourceLogLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetResourceLogLevel)

responseSendDataToWirelessDevice :: SendDataToWirelessDeviceResponse -> TestTree
responseSendDataToWirelessDevice =
  res
    "SendDataToWirelessDeviceResponse"
    "fixture/SendDataToWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendDataToWirelessDevice)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestWirelessDevice :: TestWirelessDeviceResponse -> TestTree
responseTestWirelessDevice =
  res
    "TestWirelessDeviceResponse"
    "fixture/TestWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestWirelessDevice)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateDestination :: UpdateDestinationResponse -> TestTree
responseUpdateDestination =
  res
    "UpdateDestinationResponse"
    "fixture/UpdateDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDestination)

responseUpdateLogLevelsByResourceTypes :: UpdateLogLevelsByResourceTypesResponse -> TestTree
responseUpdateLogLevelsByResourceTypes =
  res
    "UpdateLogLevelsByResourceTypesResponse"
    "fixture/UpdateLogLevelsByResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLogLevelsByResourceTypes)

responseUpdatePartnerAccount :: UpdatePartnerAccountResponse -> TestTree
responseUpdatePartnerAccount =
  res
    "UpdatePartnerAccountResponse"
    "fixture/UpdatePartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePartnerAccount)

responseUpdateWirelessDevice :: UpdateWirelessDeviceResponse -> TestTree
responseUpdateWirelessDevice =
  res
    "UpdateWirelessDeviceResponse"
    "fixture/UpdateWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWirelessDevice)

responseUpdateWirelessGateway :: UpdateWirelessGatewayResponse -> TestTree
responseUpdateWirelessGateway =
  res
    "UpdateWirelessGatewayResponse"
    "fixture/UpdateWirelessGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWirelessGateway)
