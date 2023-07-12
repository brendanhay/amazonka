{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTWireless
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         , requestAssociateMulticastGroupWithFuotaTask $
--             newAssociateMulticastGroupWithFuotaTask
--
--         , requestAssociateWirelessDeviceWithFuotaTask $
--             newAssociateWirelessDeviceWithFuotaTask
--
--         , requestAssociateWirelessDeviceWithMulticastGroup $
--             newAssociateWirelessDeviceWithMulticastGroup
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
--         , requestCancelMulticastGroupSession $
--             newCancelMulticastGroupSession
--
--         , requestCreateDestination $
--             newCreateDestination
--
--         , requestCreateDeviceProfile $
--             newCreateDeviceProfile
--
--         , requestCreateFuotaTask $
--             newCreateFuotaTask
--
--         , requestCreateMulticastGroup $
--             newCreateMulticastGroup
--
--         , requestCreateNetworkAnalyzerConfiguration $
--             newCreateNetworkAnalyzerConfiguration
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
--         , requestDeleteFuotaTask $
--             newDeleteFuotaTask
--
--         , requestDeleteMulticastGroup $
--             newDeleteMulticastGroup
--
--         , requestDeleteNetworkAnalyzerConfiguration $
--             newDeleteNetworkAnalyzerConfiguration
--
--         , requestDeleteQueuedMessages $
--             newDeleteQueuedMessages
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
--         , requestDisassociateMulticastGroupFromFuotaTask $
--             newDisassociateMulticastGroupFromFuotaTask
--
--         , requestDisassociateWirelessDeviceFromFuotaTask $
--             newDisassociateWirelessDeviceFromFuotaTask
--
--         , requestDisassociateWirelessDeviceFromMulticastGroup $
--             newDisassociateWirelessDeviceFromMulticastGroup
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
--         , requestGetEventConfigurationByResourceTypes $
--             newGetEventConfigurationByResourceTypes
--
--         , requestGetFuotaTask $
--             newGetFuotaTask
--
--         , requestGetLogLevelsByResourceTypes $
--             newGetLogLevelsByResourceTypes
--
--         , requestGetMulticastGroup $
--             newGetMulticastGroup
--
--         , requestGetMulticastGroupSession $
--             newGetMulticastGroupSession
--
--         , requestGetNetworkAnalyzerConfiguration $
--             newGetNetworkAnalyzerConfiguration
--
--         , requestGetPartnerAccount $
--             newGetPartnerAccount
--
--         , requestGetPositionEstimate $
--             newGetPositionEstimate
--
--         , requestGetResourceEventConfiguration $
--             newGetResourceEventConfiguration
--
--         , requestGetResourceLogLevel $
--             newGetResourceLogLevel
--
--         , requestGetResourcePosition $
--             newGetResourcePosition
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
--         , requestListEventConfigurations $
--             newListEventConfigurations
--
--         , requestListFuotaTasks $
--             newListFuotaTasks
--
--         , requestListMulticastGroups $
--             newListMulticastGroups
--
--         , requestListMulticastGroupsByFuotaTask $
--             newListMulticastGroupsByFuotaTask
--
--         , requestListNetworkAnalyzerConfigurations $
--             newListNetworkAnalyzerConfigurations
--
--         , requestListPartnerAccounts $
--             newListPartnerAccounts
--
--         , requestListQueuedMessages $
--             newListQueuedMessages
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
--         , requestSendDataToMulticastGroup $
--             newSendDataToMulticastGroup
--
--         , requestSendDataToWirelessDevice $
--             newSendDataToWirelessDevice
--
--         , requestStartBulkAssociateWirelessDeviceWithMulticastGroup $
--             newStartBulkAssociateWirelessDeviceWithMulticastGroup
--
--         , requestStartBulkDisassociateWirelessDeviceFromMulticastGroup $
--             newStartBulkDisassociateWirelessDeviceFromMulticastGroup
--
--         , requestStartFuotaTask $
--             newStartFuotaTask
--
--         , requestStartMulticastGroupSession $
--             newStartMulticastGroupSession
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
--         , requestUpdateEventConfigurationByResourceTypes $
--             newUpdateEventConfigurationByResourceTypes
--
--         , requestUpdateFuotaTask $
--             newUpdateFuotaTask
--
--         , requestUpdateLogLevelsByResourceTypes $
--             newUpdateLogLevelsByResourceTypes
--
--         , requestUpdateMulticastGroup $
--             newUpdateMulticastGroup
--
--         , requestUpdateNetworkAnalyzerConfiguration $
--             newUpdateNetworkAnalyzerConfiguration
--
--         , requestUpdatePartnerAccount $
--             newUpdatePartnerAccount
--
--         , requestUpdateResourceEventConfiguration $
--             newUpdateResourceEventConfiguration
--
--         , requestUpdateResourcePosition $
--             newUpdateResourcePosition
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
--         , responseAssociateMulticastGroupWithFuotaTask $
--             newAssociateMulticastGroupWithFuotaTaskResponse
--
--         , responseAssociateWirelessDeviceWithFuotaTask $
--             newAssociateWirelessDeviceWithFuotaTaskResponse
--
--         , responseAssociateWirelessDeviceWithMulticastGroup $
--             newAssociateWirelessDeviceWithMulticastGroupResponse
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
--         , responseCancelMulticastGroupSession $
--             newCancelMulticastGroupSessionResponse
--
--         , responseCreateDestination $
--             newCreateDestinationResponse
--
--         , responseCreateDeviceProfile $
--             newCreateDeviceProfileResponse
--
--         , responseCreateFuotaTask $
--             newCreateFuotaTaskResponse
--
--         , responseCreateMulticastGroup $
--             newCreateMulticastGroupResponse
--
--         , responseCreateNetworkAnalyzerConfiguration $
--             newCreateNetworkAnalyzerConfigurationResponse
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
--         , responseDeleteFuotaTask $
--             newDeleteFuotaTaskResponse
--
--         , responseDeleteMulticastGroup $
--             newDeleteMulticastGroupResponse
--
--         , responseDeleteNetworkAnalyzerConfiguration $
--             newDeleteNetworkAnalyzerConfigurationResponse
--
--         , responseDeleteQueuedMessages $
--             newDeleteQueuedMessagesResponse
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
--         , responseDisassociateMulticastGroupFromFuotaTask $
--             newDisassociateMulticastGroupFromFuotaTaskResponse
--
--         , responseDisassociateWirelessDeviceFromFuotaTask $
--             newDisassociateWirelessDeviceFromFuotaTaskResponse
--
--         , responseDisassociateWirelessDeviceFromMulticastGroup $
--             newDisassociateWirelessDeviceFromMulticastGroupResponse
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
--         , responseGetEventConfigurationByResourceTypes $
--             newGetEventConfigurationByResourceTypesResponse
--
--         , responseGetFuotaTask $
--             newGetFuotaTaskResponse
--
--         , responseGetLogLevelsByResourceTypes $
--             newGetLogLevelsByResourceTypesResponse
--
--         , responseGetMulticastGroup $
--             newGetMulticastGroupResponse
--
--         , responseGetMulticastGroupSession $
--             newGetMulticastGroupSessionResponse
--
--         , responseGetNetworkAnalyzerConfiguration $
--             newGetNetworkAnalyzerConfigurationResponse
--
--         , responseGetPartnerAccount $
--             newGetPartnerAccountResponse
--
--         , responseGetPositionEstimate $
--             newGetPositionEstimateResponse
--
--         , responseGetResourceEventConfiguration $
--             newGetResourceEventConfigurationResponse
--
--         , responseGetResourceLogLevel $
--             newGetResourceLogLevelResponse
--
--         , responseGetResourcePosition $
--             newGetResourcePositionResponse
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
--         , responseListEventConfigurations $
--             newListEventConfigurationsResponse
--
--         , responseListFuotaTasks $
--             newListFuotaTasksResponse
--
--         , responseListMulticastGroups $
--             newListMulticastGroupsResponse
--
--         , responseListMulticastGroupsByFuotaTask $
--             newListMulticastGroupsByFuotaTaskResponse
--
--         , responseListNetworkAnalyzerConfigurations $
--             newListNetworkAnalyzerConfigurationsResponse
--
--         , responseListPartnerAccounts $
--             newListPartnerAccountsResponse
--
--         , responseListQueuedMessages $
--             newListQueuedMessagesResponse
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
--         , responseSendDataToMulticastGroup $
--             newSendDataToMulticastGroupResponse
--
--         , responseSendDataToWirelessDevice $
--             newSendDataToWirelessDeviceResponse
--
--         , responseStartBulkAssociateWirelessDeviceWithMulticastGroup $
--             newStartBulkAssociateWirelessDeviceWithMulticastGroupResponse
--
--         , responseStartBulkDisassociateWirelessDeviceFromMulticastGroup $
--             newStartBulkDisassociateWirelessDeviceFromMulticastGroupResponse
--
--         , responseStartFuotaTask $
--             newStartFuotaTaskResponse
--
--         , responseStartMulticastGroupSession $
--             newStartMulticastGroupSessionResponse
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
--         , responseUpdateEventConfigurationByResourceTypes $
--             newUpdateEventConfigurationByResourceTypesResponse
--
--         , responseUpdateFuotaTask $
--             newUpdateFuotaTaskResponse
--
--         , responseUpdateLogLevelsByResourceTypes $
--             newUpdateLogLevelsByResourceTypesResponse
--
--         , responseUpdateMulticastGroup $
--             newUpdateMulticastGroupResponse
--
--         , responseUpdateNetworkAnalyzerConfiguration $
--             newUpdateNetworkAnalyzerConfigurationResponse
--
--         , responseUpdatePartnerAccount $
--             newUpdatePartnerAccountResponse
--
--         , responseUpdateResourceEventConfiguration $
--             newUpdateResourceEventConfigurationResponse
--
--         , responseUpdateResourcePosition $
--             newUpdateResourcePositionResponse
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

requestAssociateMulticastGroupWithFuotaTask :: AssociateMulticastGroupWithFuotaTask -> TestTree
requestAssociateMulticastGroupWithFuotaTask =
  req
    "AssociateMulticastGroupWithFuotaTask"
    "fixture/AssociateMulticastGroupWithFuotaTask.yaml"

requestAssociateWirelessDeviceWithFuotaTask :: AssociateWirelessDeviceWithFuotaTask -> TestTree
requestAssociateWirelessDeviceWithFuotaTask =
  req
    "AssociateWirelessDeviceWithFuotaTask"
    "fixture/AssociateWirelessDeviceWithFuotaTask.yaml"

requestAssociateWirelessDeviceWithMulticastGroup :: AssociateWirelessDeviceWithMulticastGroup -> TestTree
requestAssociateWirelessDeviceWithMulticastGroup =
  req
    "AssociateWirelessDeviceWithMulticastGroup"
    "fixture/AssociateWirelessDeviceWithMulticastGroup.yaml"

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

requestCancelMulticastGroupSession :: CancelMulticastGroupSession -> TestTree
requestCancelMulticastGroupSession =
  req
    "CancelMulticastGroupSession"
    "fixture/CancelMulticastGroupSession.yaml"

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

requestCreateFuotaTask :: CreateFuotaTask -> TestTree
requestCreateFuotaTask =
  req
    "CreateFuotaTask"
    "fixture/CreateFuotaTask.yaml"

requestCreateMulticastGroup :: CreateMulticastGroup -> TestTree
requestCreateMulticastGroup =
  req
    "CreateMulticastGroup"
    "fixture/CreateMulticastGroup.yaml"

requestCreateNetworkAnalyzerConfiguration :: CreateNetworkAnalyzerConfiguration -> TestTree
requestCreateNetworkAnalyzerConfiguration =
  req
    "CreateNetworkAnalyzerConfiguration"
    "fixture/CreateNetworkAnalyzerConfiguration.yaml"

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

requestDeleteFuotaTask :: DeleteFuotaTask -> TestTree
requestDeleteFuotaTask =
  req
    "DeleteFuotaTask"
    "fixture/DeleteFuotaTask.yaml"

requestDeleteMulticastGroup :: DeleteMulticastGroup -> TestTree
requestDeleteMulticastGroup =
  req
    "DeleteMulticastGroup"
    "fixture/DeleteMulticastGroup.yaml"

requestDeleteNetworkAnalyzerConfiguration :: DeleteNetworkAnalyzerConfiguration -> TestTree
requestDeleteNetworkAnalyzerConfiguration =
  req
    "DeleteNetworkAnalyzerConfiguration"
    "fixture/DeleteNetworkAnalyzerConfiguration.yaml"

requestDeleteQueuedMessages :: DeleteQueuedMessages -> TestTree
requestDeleteQueuedMessages =
  req
    "DeleteQueuedMessages"
    "fixture/DeleteQueuedMessages.yaml"

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

requestDisassociateMulticastGroupFromFuotaTask :: DisassociateMulticastGroupFromFuotaTask -> TestTree
requestDisassociateMulticastGroupFromFuotaTask =
  req
    "DisassociateMulticastGroupFromFuotaTask"
    "fixture/DisassociateMulticastGroupFromFuotaTask.yaml"

requestDisassociateWirelessDeviceFromFuotaTask :: DisassociateWirelessDeviceFromFuotaTask -> TestTree
requestDisassociateWirelessDeviceFromFuotaTask =
  req
    "DisassociateWirelessDeviceFromFuotaTask"
    "fixture/DisassociateWirelessDeviceFromFuotaTask.yaml"

requestDisassociateWirelessDeviceFromMulticastGroup :: DisassociateWirelessDeviceFromMulticastGroup -> TestTree
requestDisassociateWirelessDeviceFromMulticastGroup =
  req
    "DisassociateWirelessDeviceFromMulticastGroup"
    "fixture/DisassociateWirelessDeviceFromMulticastGroup.yaml"

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

requestGetEventConfigurationByResourceTypes :: GetEventConfigurationByResourceTypes -> TestTree
requestGetEventConfigurationByResourceTypes =
  req
    "GetEventConfigurationByResourceTypes"
    "fixture/GetEventConfigurationByResourceTypes.yaml"

requestGetFuotaTask :: GetFuotaTask -> TestTree
requestGetFuotaTask =
  req
    "GetFuotaTask"
    "fixture/GetFuotaTask.yaml"

requestGetLogLevelsByResourceTypes :: GetLogLevelsByResourceTypes -> TestTree
requestGetLogLevelsByResourceTypes =
  req
    "GetLogLevelsByResourceTypes"
    "fixture/GetLogLevelsByResourceTypes.yaml"

requestGetMulticastGroup :: GetMulticastGroup -> TestTree
requestGetMulticastGroup =
  req
    "GetMulticastGroup"
    "fixture/GetMulticastGroup.yaml"

requestGetMulticastGroupSession :: GetMulticastGroupSession -> TestTree
requestGetMulticastGroupSession =
  req
    "GetMulticastGroupSession"
    "fixture/GetMulticastGroupSession.yaml"

requestGetNetworkAnalyzerConfiguration :: GetNetworkAnalyzerConfiguration -> TestTree
requestGetNetworkAnalyzerConfiguration =
  req
    "GetNetworkAnalyzerConfiguration"
    "fixture/GetNetworkAnalyzerConfiguration.yaml"

requestGetPartnerAccount :: GetPartnerAccount -> TestTree
requestGetPartnerAccount =
  req
    "GetPartnerAccount"
    "fixture/GetPartnerAccount.yaml"

requestGetPositionEstimate :: GetPositionEstimate -> TestTree
requestGetPositionEstimate =
  req
    "GetPositionEstimate"
    "fixture/GetPositionEstimate.yaml"

requestGetResourceEventConfiguration :: GetResourceEventConfiguration -> TestTree
requestGetResourceEventConfiguration =
  req
    "GetResourceEventConfiguration"
    "fixture/GetResourceEventConfiguration.yaml"

requestGetResourceLogLevel :: GetResourceLogLevel -> TestTree
requestGetResourceLogLevel =
  req
    "GetResourceLogLevel"
    "fixture/GetResourceLogLevel.yaml"

requestGetResourcePosition :: GetResourcePosition -> TestTree
requestGetResourcePosition =
  req
    "GetResourcePosition"
    "fixture/GetResourcePosition.yaml"

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

requestListEventConfigurations :: ListEventConfigurations -> TestTree
requestListEventConfigurations =
  req
    "ListEventConfigurations"
    "fixture/ListEventConfigurations.yaml"

requestListFuotaTasks :: ListFuotaTasks -> TestTree
requestListFuotaTasks =
  req
    "ListFuotaTasks"
    "fixture/ListFuotaTasks.yaml"

requestListMulticastGroups :: ListMulticastGroups -> TestTree
requestListMulticastGroups =
  req
    "ListMulticastGroups"
    "fixture/ListMulticastGroups.yaml"

requestListMulticastGroupsByFuotaTask :: ListMulticastGroupsByFuotaTask -> TestTree
requestListMulticastGroupsByFuotaTask =
  req
    "ListMulticastGroupsByFuotaTask"
    "fixture/ListMulticastGroupsByFuotaTask.yaml"

requestListNetworkAnalyzerConfigurations :: ListNetworkAnalyzerConfigurations -> TestTree
requestListNetworkAnalyzerConfigurations =
  req
    "ListNetworkAnalyzerConfigurations"
    "fixture/ListNetworkAnalyzerConfigurations.yaml"

requestListPartnerAccounts :: ListPartnerAccounts -> TestTree
requestListPartnerAccounts =
  req
    "ListPartnerAccounts"
    "fixture/ListPartnerAccounts.yaml"

requestListQueuedMessages :: ListQueuedMessages -> TestTree
requestListQueuedMessages =
  req
    "ListQueuedMessages"
    "fixture/ListQueuedMessages.yaml"

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

requestSendDataToMulticastGroup :: SendDataToMulticastGroup -> TestTree
requestSendDataToMulticastGroup =
  req
    "SendDataToMulticastGroup"
    "fixture/SendDataToMulticastGroup.yaml"

requestSendDataToWirelessDevice :: SendDataToWirelessDevice -> TestTree
requestSendDataToWirelessDevice =
  req
    "SendDataToWirelessDevice"
    "fixture/SendDataToWirelessDevice.yaml"

requestStartBulkAssociateWirelessDeviceWithMulticastGroup :: StartBulkAssociateWirelessDeviceWithMulticastGroup -> TestTree
requestStartBulkAssociateWirelessDeviceWithMulticastGroup =
  req
    "StartBulkAssociateWirelessDeviceWithMulticastGroup"
    "fixture/StartBulkAssociateWirelessDeviceWithMulticastGroup.yaml"

requestStartBulkDisassociateWirelessDeviceFromMulticastGroup :: StartBulkDisassociateWirelessDeviceFromMulticastGroup -> TestTree
requestStartBulkDisassociateWirelessDeviceFromMulticastGroup =
  req
    "StartBulkDisassociateWirelessDeviceFromMulticastGroup"
    "fixture/StartBulkDisassociateWirelessDeviceFromMulticastGroup.yaml"

requestStartFuotaTask :: StartFuotaTask -> TestTree
requestStartFuotaTask =
  req
    "StartFuotaTask"
    "fixture/StartFuotaTask.yaml"

requestStartMulticastGroupSession :: StartMulticastGroupSession -> TestTree
requestStartMulticastGroupSession =
  req
    "StartMulticastGroupSession"
    "fixture/StartMulticastGroupSession.yaml"

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

requestUpdateEventConfigurationByResourceTypes :: UpdateEventConfigurationByResourceTypes -> TestTree
requestUpdateEventConfigurationByResourceTypes =
  req
    "UpdateEventConfigurationByResourceTypes"
    "fixture/UpdateEventConfigurationByResourceTypes.yaml"

requestUpdateFuotaTask :: UpdateFuotaTask -> TestTree
requestUpdateFuotaTask =
  req
    "UpdateFuotaTask"
    "fixture/UpdateFuotaTask.yaml"

requestUpdateLogLevelsByResourceTypes :: UpdateLogLevelsByResourceTypes -> TestTree
requestUpdateLogLevelsByResourceTypes =
  req
    "UpdateLogLevelsByResourceTypes"
    "fixture/UpdateLogLevelsByResourceTypes.yaml"

requestUpdateMulticastGroup :: UpdateMulticastGroup -> TestTree
requestUpdateMulticastGroup =
  req
    "UpdateMulticastGroup"
    "fixture/UpdateMulticastGroup.yaml"

requestUpdateNetworkAnalyzerConfiguration :: UpdateNetworkAnalyzerConfiguration -> TestTree
requestUpdateNetworkAnalyzerConfiguration =
  req
    "UpdateNetworkAnalyzerConfiguration"
    "fixture/UpdateNetworkAnalyzerConfiguration.yaml"

requestUpdatePartnerAccount :: UpdatePartnerAccount -> TestTree
requestUpdatePartnerAccount =
  req
    "UpdatePartnerAccount"
    "fixture/UpdatePartnerAccount.yaml"

requestUpdateResourceEventConfiguration :: UpdateResourceEventConfiguration -> TestTree
requestUpdateResourceEventConfiguration =
  req
    "UpdateResourceEventConfiguration"
    "fixture/UpdateResourceEventConfiguration.yaml"

requestUpdateResourcePosition :: UpdateResourcePosition -> TestTree
requestUpdateResourcePosition =
  req
    "UpdateResourcePosition"
    "fixture/UpdateResourcePosition.yaml"

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

responseAssociateMulticastGroupWithFuotaTask :: AssociateMulticastGroupWithFuotaTaskResponse -> TestTree
responseAssociateMulticastGroupWithFuotaTask =
  res
    "AssociateMulticastGroupWithFuotaTaskResponse"
    "fixture/AssociateMulticastGroupWithFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateMulticastGroupWithFuotaTask)

responseAssociateWirelessDeviceWithFuotaTask :: AssociateWirelessDeviceWithFuotaTaskResponse -> TestTree
responseAssociateWirelessDeviceWithFuotaTask =
  res
    "AssociateWirelessDeviceWithFuotaTaskResponse"
    "fixture/AssociateWirelessDeviceWithFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWirelessDeviceWithFuotaTask)

responseAssociateWirelessDeviceWithMulticastGroup :: AssociateWirelessDeviceWithMulticastGroupResponse -> TestTree
responseAssociateWirelessDeviceWithMulticastGroup =
  res
    "AssociateWirelessDeviceWithMulticastGroupResponse"
    "fixture/AssociateWirelessDeviceWithMulticastGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWirelessDeviceWithMulticastGroup)

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

responseCancelMulticastGroupSession :: CancelMulticastGroupSessionResponse -> TestTree
responseCancelMulticastGroupSession =
  res
    "CancelMulticastGroupSessionResponse"
    "fixture/CancelMulticastGroupSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelMulticastGroupSession)

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

responseCreateFuotaTask :: CreateFuotaTaskResponse -> TestTree
responseCreateFuotaTask =
  res
    "CreateFuotaTaskResponse"
    "fixture/CreateFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFuotaTask)

responseCreateMulticastGroup :: CreateMulticastGroupResponse -> TestTree
responseCreateMulticastGroup =
  res
    "CreateMulticastGroupResponse"
    "fixture/CreateMulticastGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMulticastGroup)

responseCreateNetworkAnalyzerConfiguration :: CreateNetworkAnalyzerConfigurationResponse -> TestTree
responseCreateNetworkAnalyzerConfiguration =
  res
    "CreateNetworkAnalyzerConfigurationResponse"
    "fixture/CreateNetworkAnalyzerConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNetworkAnalyzerConfiguration)

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

responseDeleteFuotaTask :: DeleteFuotaTaskResponse -> TestTree
responseDeleteFuotaTask =
  res
    "DeleteFuotaTaskResponse"
    "fixture/DeleteFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFuotaTask)

responseDeleteMulticastGroup :: DeleteMulticastGroupResponse -> TestTree
responseDeleteMulticastGroup =
  res
    "DeleteMulticastGroupResponse"
    "fixture/DeleteMulticastGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMulticastGroup)

responseDeleteNetworkAnalyzerConfiguration :: DeleteNetworkAnalyzerConfigurationResponse -> TestTree
responseDeleteNetworkAnalyzerConfiguration =
  res
    "DeleteNetworkAnalyzerConfigurationResponse"
    "fixture/DeleteNetworkAnalyzerConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNetworkAnalyzerConfiguration)

responseDeleteQueuedMessages :: DeleteQueuedMessagesResponse -> TestTree
responseDeleteQueuedMessages =
  res
    "DeleteQueuedMessagesResponse"
    "fixture/DeleteQueuedMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueuedMessages)

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

responseDisassociateMulticastGroupFromFuotaTask :: DisassociateMulticastGroupFromFuotaTaskResponse -> TestTree
responseDisassociateMulticastGroupFromFuotaTask =
  res
    "DisassociateMulticastGroupFromFuotaTaskResponse"
    "fixture/DisassociateMulticastGroupFromFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMulticastGroupFromFuotaTask)

responseDisassociateWirelessDeviceFromFuotaTask :: DisassociateWirelessDeviceFromFuotaTaskResponse -> TestTree
responseDisassociateWirelessDeviceFromFuotaTask =
  res
    "DisassociateWirelessDeviceFromFuotaTaskResponse"
    "fixture/DisassociateWirelessDeviceFromFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWirelessDeviceFromFuotaTask)

responseDisassociateWirelessDeviceFromMulticastGroup :: DisassociateWirelessDeviceFromMulticastGroupResponse -> TestTree
responseDisassociateWirelessDeviceFromMulticastGroup =
  res
    "DisassociateWirelessDeviceFromMulticastGroupResponse"
    "fixture/DisassociateWirelessDeviceFromMulticastGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWirelessDeviceFromMulticastGroup)

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

responseGetEventConfigurationByResourceTypes :: GetEventConfigurationByResourceTypesResponse -> TestTree
responseGetEventConfigurationByResourceTypes =
  res
    "GetEventConfigurationByResourceTypesResponse"
    "fixture/GetEventConfigurationByResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventConfigurationByResourceTypes)

responseGetFuotaTask :: GetFuotaTaskResponse -> TestTree
responseGetFuotaTask =
  res
    "GetFuotaTaskResponse"
    "fixture/GetFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFuotaTask)

responseGetLogLevelsByResourceTypes :: GetLogLevelsByResourceTypesResponse -> TestTree
responseGetLogLevelsByResourceTypes =
  res
    "GetLogLevelsByResourceTypesResponse"
    "fixture/GetLogLevelsByResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLogLevelsByResourceTypes)

responseGetMulticastGroup :: GetMulticastGroupResponse -> TestTree
responseGetMulticastGroup =
  res
    "GetMulticastGroupResponse"
    "fixture/GetMulticastGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMulticastGroup)

responseGetMulticastGroupSession :: GetMulticastGroupSessionResponse -> TestTree
responseGetMulticastGroupSession =
  res
    "GetMulticastGroupSessionResponse"
    "fixture/GetMulticastGroupSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMulticastGroupSession)

responseGetNetworkAnalyzerConfiguration :: GetNetworkAnalyzerConfigurationResponse -> TestTree
responseGetNetworkAnalyzerConfiguration =
  res
    "GetNetworkAnalyzerConfigurationResponse"
    "fixture/GetNetworkAnalyzerConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNetworkAnalyzerConfiguration)

responseGetPartnerAccount :: GetPartnerAccountResponse -> TestTree
responseGetPartnerAccount =
  res
    "GetPartnerAccountResponse"
    "fixture/GetPartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPartnerAccount)

responseGetPositionEstimate :: GetPositionEstimateResponse -> TestTree
responseGetPositionEstimate =
  res
    "GetPositionEstimateResponse"
    "fixture/GetPositionEstimateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPositionEstimate)

responseGetResourceEventConfiguration :: GetResourceEventConfigurationResponse -> TestTree
responseGetResourceEventConfiguration =
  res
    "GetResourceEventConfigurationResponse"
    "fixture/GetResourceEventConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceEventConfiguration)

responseGetResourceLogLevel :: GetResourceLogLevelResponse -> TestTree
responseGetResourceLogLevel =
  res
    "GetResourceLogLevelResponse"
    "fixture/GetResourceLogLevelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceLogLevel)

responseGetResourcePosition :: GetResourcePositionResponse -> TestTree
responseGetResourcePosition =
  res
    "GetResourcePositionResponse"
    "fixture/GetResourcePositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePosition)

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

responseListEventConfigurations :: ListEventConfigurationsResponse -> TestTree
responseListEventConfigurations =
  res
    "ListEventConfigurationsResponse"
    "fixture/ListEventConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventConfigurations)

responseListFuotaTasks :: ListFuotaTasksResponse -> TestTree
responseListFuotaTasks =
  res
    "ListFuotaTasksResponse"
    "fixture/ListFuotaTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFuotaTasks)

responseListMulticastGroups :: ListMulticastGroupsResponse -> TestTree
responseListMulticastGroups =
  res
    "ListMulticastGroupsResponse"
    "fixture/ListMulticastGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMulticastGroups)

responseListMulticastGroupsByFuotaTask :: ListMulticastGroupsByFuotaTaskResponse -> TestTree
responseListMulticastGroupsByFuotaTask =
  res
    "ListMulticastGroupsByFuotaTaskResponse"
    "fixture/ListMulticastGroupsByFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMulticastGroupsByFuotaTask)

responseListNetworkAnalyzerConfigurations :: ListNetworkAnalyzerConfigurationsResponse -> TestTree
responseListNetworkAnalyzerConfigurations =
  res
    "ListNetworkAnalyzerConfigurationsResponse"
    "fixture/ListNetworkAnalyzerConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNetworkAnalyzerConfigurations)

responseListPartnerAccounts :: ListPartnerAccountsResponse -> TestTree
responseListPartnerAccounts =
  res
    "ListPartnerAccountsResponse"
    "fixture/ListPartnerAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPartnerAccounts)

responseListQueuedMessages :: ListQueuedMessagesResponse -> TestTree
responseListQueuedMessages =
  res
    "ListQueuedMessagesResponse"
    "fixture/ListQueuedMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueuedMessages)

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

responseSendDataToMulticastGroup :: SendDataToMulticastGroupResponse -> TestTree
responseSendDataToMulticastGroup =
  res
    "SendDataToMulticastGroupResponse"
    "fixture/SendDataToMulticastGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendDataToMulticastGroup)

responseSendDataToWirelessDevice :: SendDataToWirelessDeviceResponse -> TestTree
responseSendDataToWirelessDevice =
  res
    "SendDataToWirelessDeviceResponse"
    "fixture/SendDataToWirelessDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendDataToWirelessDevice)

responseStartBulkAssociateWirelessDeviceWithMulticastGroup :: StartBulkAssociateWirelessDeviceWithMulticastGroupResponse -> TestTree
responseStartBulkAssociateWirelessDeviceWithMulticastGroup =
  res
    "StartBulkAssociateWirelessDeviceWithMulticastGroupResponse"
    "fixture/StartBulkAssociateWirelessDeviceWithMulticastGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBulkAssociateWirelessDeviceWithMulticastGroup)

responseStartBulkDisassociateWirelessDeviceFromMulticastGroup :: StartBulkDisassociateWirelessDeviceFromMulticastGroupResponse -> TestTree
responseStartBulkDisassociateWirelessDeviceFromMulticastGroup =
  res
    "StartBulkDisassociateWirelessDeviceFromMulticastGroupResponse"
    "fixture/StartBulkDisassociateWirelessDeviceFromMulticastGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBulkDisassociateWirelessDeviceFromMulticastGroup)

responseStartFuotaTask :: StartFuotaTaskResponse -> TestTree
responseStartFuotaTask =
  res
    "StartFuotaTaskResponse"
    "fixture/StartFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFuotaTask)

responseStartMulticastGroupSession :: StartMulticastGroupSessionResponse -> TestTree
responseStartMulticastGroupSession =
  res
    "StartMulticastGroupSessionResponse"
    "fixture/StartMulticastGroupSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMulticastGroupSession)

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

responseUpdateEventConfigurationByResourceTypes :: UpdateEventConfigurationByResourceTypesResponse -> TestTree
responseUpdateEventConfigurationByResourceTypes =
  res
    "UpdateEventConfigurationByResourceTypesResponse"
    "fixture/UpdateEventConfigurationByResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventConfigurationByResourceTypes)

responseUpdateFuotaTask :: UpdateFuotaTaskResponse -> TestTree
responseUpdateFuotaTask =
  res
    "UpdateFuotaTaskResponse"
    "fixture/UpdateFuotaTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFuotaTask)

responseUpdateLogLevelsByResourceTypes :: UpdateLogLevelsByResourceTypesResponse -> TestTree
responseUpdateLogLevelsByResourceTypes =
  res
    "UpdateLogLevelsByResourceTypesResponse"
    "fixture/UpdateLogLevelsByResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLogLevelsByResourceTypes)

responseUpdateMulticastGroup :: UpdateMulticastGroupResponse -> TestTree
responseUpdateMulticastGroup =
  res
    "UpdateMulticastGroupResponse"
    "fixture/UpdateMulticastGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMulticastGroup)

responseUpdateNetworkAnalyzerConfiguration :: UpdateNetworkAnalyzerConfigurationResponse -> TestTree
responseUpdateNetworkAnalyzerConfiguration =
  res
    "UpdateNetworkAnalyzerConfigurationResponse"
    "fixture/UpdateNetworkAnalyzerConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNetworkAnalyzerConfiguration)

responseUpdatePartnerAccount :: UpdatePartnerAccountResponse -> TestTree
responseUpdatePartnerAccount =
  res
    "UpdatePartnerAccountResponse"
    "fixture/UpdatePartnerAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePartnerAccount)

responseUpdateResourceEventConfiguration :: UpdateResourceEventConfigurationResponse -> TestTree
responseUpdateResourceEventConfiguration =
  res
    "UpdateResourceEventConfigurationResponse"
    "fixture/UpdateResourceEventConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceEventConfiguration)

responseUpdateResourcePosition :: UpdateResourcePositionResponse -> TestTree
responseUpdateResourcePosition =
  res
    "UpdateResourcePositionResponse"
    "fixture/UpdateResourcePositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourcePosition)

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
