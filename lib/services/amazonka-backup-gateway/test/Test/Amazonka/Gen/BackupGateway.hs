{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.BackupGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.BackupGateway where

import Amazonka.BackupGateway
import qualified Data.Proxy as Proxy
import Test.Amazonka.BackupGateway.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateGatewayToServer $
--             newAssociateGatewayToServer
--
--         , requestCreateGateway $
--             newCreateGateway
--
--         , requestDeleteGateway $
--             newDeleteGateway
--
--         , requestDeleteHypervisor $
--             newDeleteHypervisor
--
--         , requestDisassociateGatewayFromServer $
--             newDisassociateGatewayFromServer
--
--         , requestGetBandwidthRateLimitSchedule $
--             newGetBandwidthRateLimitSchedule
--
--         , requestGetGateway $
--             newGetGateway
--
--         , requestGetHypervisor $
--             newGetHypervisor
--
--         , requestGetHypervisorPropertyMappings $
--             newGetHypervisorPropertyMappings
--
--         , requestGetVirtualMachine $
--             newGetVirtualMachine
--
--         , requestImportHypervisorConfiguration $
--             newImportHypervisorConfiguration
--
--         , requestListGateways $
--             newListGateways
--
--         , requestListHypervisors $
--             newListHypervisors
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListVirtualMachines $
--             newListVirtualMachines
--
--         , requestPutBandwidthRateLimitSchedule $
--             newPutBandwidthRateLimitSchedule
--
--         , requestPutHypervisorPropertyMappings $
--             newPutHypervisorPropertyMappings
--
--         , requestPutMaintenanceStartTime $
--             newPutMaintenanceStartTime
--
--         , requestStartVirtualMachinesMetadataSync $
--             newStartVirtualMachinesMetadataSync
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestHypervisorConfiguration $
--             newTestHypervisorConfiguration
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateGatewayInformation $
--             newUpdateGatewayInformation
--
--         , requestUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNow
--
--         , requestUpdateHypervisor $
--             newUpdateHypervisor
--
--           ]

--     , testGroup "response"
--         [ responseAssociateGatewayToServer $
--             newAssociateGatewayToServerResponse
--
--         , responseCreateGateway $
--             newCreateGatewayResponse
--
--         , responseDeleteGateway $
--             newDeleteGatewayResponse
--
--         , responseDeleteHypervisor $
--             newDeleteHypervisorResponse
--
--         , responseDisassociateGatewayFromServer $
--             newDisassociateGatewayFromServerResponse
--
--         , responseGetBandwidthRateLimitSchedule $
--             newGetBandwidthRateLimitScheduleResponse
--
--         , responseGetGateway $
--             newGetGatewayResponse
--
--         , responseGetHypervisor $
--             newGetHypervisorResponse
--
--         , responseGetHypervisorPropertyMappings $
--             newGetHypervisorPropertyMappingsResponse
--
--         , responseGetVirtualMachine $
--             newGetVirtualMachineResponse
--
--         , responseImportHypervisorConfiguration $
--             newImportHypervisorConfigurationResponse
--
--         , responseListGateways $
--             newListGatewaysResponse
--
--         , responseListHypervisors $
--             newListHypervisorsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListVirtualMachines $
--             newListVirtualMachinesResponse
--
--         , responsePutBandwidthRateLimitSchedule $
--             newPutBandwidthRateLimitScheduleResponse
--
--         , responsePutHypervisorPropertyMappings $
--             newPutHypervisorPropertyMappingsResponse
--
--         , responsePutMaintenanceStartTime $
--             newPutMaintenanceStartTimeResponse
--
--         , responseStartVirtualMachinesMetadataSync $
--             newStartVirtualMachinesMetadataSyncResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestHypervisorConfiguration $
--             newTestHypervisorConfigurationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateGatewayInformation $
--             newUpdateGatewayInformationResponse
--
--         , responseUpdateGatewaySoftwareNow $
--             newUpdateGatewaySoftwareNowResponse
--
--         , responseUpdateHypervisor $
--             newUpdateHypervisorResponse
--
--           ]
--     ]

-- Requests

requestAssociateGatewayToServer :: AssociateGatewayToServer -> TestTree
requestAssociateGatewayToServer =
  req
    "AssociateGatewayToServer"
    "fixture/AssociateGatewayToServer.yaml"

requestCreateGateway :: CreateGateway -> TestTree
requestCreateGateway =
  req
    "CreateGateway"
    "fixture/CreateGateway.yaml"

requestDeleteGateway :: DeleteGateway -> TestTree
requestDeleteGateway =
  req
    "DeleteGateway"
    "fixture/DeleteGateway.yaml"

requestDeleteHypervisor :: DeleteHypervisor -> TestTree
requestDeleteHypervisor =
  req
    "DeleteHypervisor"
    "fixture/DeleteHypervisor.yaml"

requestDisassociateGatewayFromServer :: DisassociateGatewayFromServer -> TestTree
requestDisassociateGatewayFromServer =
  req
    "DisassociateGatewayFromServer"
    "fixture/DisassociateGatewayFromServer.yaml"

requestGetBandwidthRateLimitSchedule :: GetBandwidthRateLimitSchedule -> TestTree
requestGetBandwidthRateLimitSchedule =
  req
    "GetBandwidthRateLimitSchedule"
    "fixture/GetBandwidthRateLimitSchedule.yaml"

requestGetGateway :: GetGateway -> TestTree
requestGetGateway =
  req
    "GetGateway"
    "fixture/GetGateway.yaml"

requestGetHypervisor :: GetHypervisor -> TestTree
requestGetHypervisor =
  req
    "GetHypervisor"
    "fixture/GetHypervisor.yaml"

requestGetHypervisorPropertyMappings :: GetHypervisorPropertyMappings -> TestTree
requestGetHypervisorPropertyMappings =
  req
    "GetHypervisorPropertyMappings"
    "fixture/GetHypervisorPropertyMappings.yaml"

requestGetVirtualMachine :: GetVirtualMachine -> TestTree
requestGetVirtualMachine =
  req
    "GetVirtualMachine"
    "fixture/GetVirtualMachine.yaml"

requestImportHypervisorConfiguration :: ImportHypervisorConfiguration -> TestTree
requestImportHypervisorConfiguration =
  req
    "ImportHypervisorConfiguration"
    "fixture/ImportHypervisorConfiguration.yaml"

requestListGateways :: ListGateways -> TestTree
requestListGateways =
  req
    "ListGateways"
    "fixture/ListGateways.yaml"

requestListHypervisors :: ListHypervisors -> TestTree
requestListHypervisors =
  req
    "ListHypervisors"
    "fixture/ListHypervisors.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListVirtualMachines :: ListVirtualMachines -> TestTree
requestListVirtualMachines =
  req
    "ListVirtualMachines"
    "fixture/ListVirtualMachines.yaml"

requestPutBandwidthRateLimitSchedule :: PutBandwidthRateLimitSchedule -> TestTree
requestPutBandwidthRateLimitSchedule =
  req
    "PutBandwidthRateLimitSchedule"
    "fixture/PutBandwidthRateLimitSchedule.yaml"

requestPutHypervisorPropertyMappings :: PutHypervisorPropertyMappings -> TestTree
requestPutHypervisorPropertyMappings =
  req
    "PutHypervisorPropertyMappings"
    "fixture/PutHypervisorPropertyMappings.yaml"

requestPutMaintenanceStartTime :: PutMaintenanceStartTime -> TestTree
requestPutMaintenanceStartTime =
  req
    "PutMaintenanceStartTime"
    "fixture/PutMaintenanceStartTime.yaml"

requestStartVirtualMachinesMetadataSync :: StartVirtualMachinesMetadataSync -> TestTree
requestStartVirtualMachinesMetadataSync =
  req
    "StartVirtualMachinesMetadataSync"
    "fixture/StartVirtualMachinesMetadataSync.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestHypervisorConfiguration :: TestHypervisorConfiguration -> TestTree
requestTestHypervisorConfiguration =
  req
    "TestHypervisorConfiguration"
    "fixture/TestHypervisorConfiguration.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateGatewayInformation :: UpdateGatewayInformation -> TestTree
requestUpdateGatewayInformation =
  req
    "UpdateGatewayInformation"
    "fixture/UpdateGatewayInformation.yaml"

requestUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNow -> TestTree
requestUpdateGatewaySoftwareNow =
  req
    "UpdateGatewaySoftwareNow"
    "fixture/UpdateGatewaySoftwareNow.yaml"

requestUpdateHypervisor :: UpdateHypervisor -> TestTree
requestUpdateHypervisor =
  req
    "UpdateHypervisor"
    "fixture/UpdateHypervisor.yaml"

-- Responses

responseAssociateGatewayToServer :: AssociateGatewayToServerResponse -> TestTree
responseAssociateGatewayToServer =
  res
    "AssociateGatewayToServerResponse"
    "fixture/AssociateGatewayToServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateGatewayToServer)

responseCreateGateway :: CreateGatewayResponse -> TestTree
responseCreateGateway =
  res
    "CreateGatewayResponse"
    "fixture/CreateGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGateway)

responseDeleteGateway :: DeleteGatewayResponse -> TestTree
responseDeleteGateway =
  res
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGateway)

responseDeleteHypervisor :: DeleteHypervisorResponse -> TestTree
responseDeleteHypervisor =
  res
    "DeleteHypervisorResponse"
    "fixture/DeleteHypervisorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHypervisor)

responseDisassociateGatewayFromServer :: DisassociateGatewayFromServerResponse -> TestTree
responseDisassociateGatewayFromServer =
  res
    "DisassociateGatewayFromServerResponse"
    "fixture/DisassociateGatewayFromServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateGatewayFromServer)

responseGetBandwidthRateLimitSchedule :: GetBandwidthRateLimitScheduleResponse -> TestTree
responseGetBandwidthRateLimitSchedule =
  res
    "GetBandwidthRateLimitScheduleResponse"
    "fixture/GetBandwidthRateLimitScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBandwidthRateLimitSchedule)

responseGetGateway :: GetGatewayResponse -> TestTree
responseGetGateway =
  res
    "GetGatewayResponse"
    "fixture/GetGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGateway)

responseGetHypervisor :: GetHypervisorResponse -> TestTree
responseGetHypervisor =
  res
    "GetHypervisorResponse"
    "fixture/GetHypervisorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHypervisor)

responseGetHypervisorPropertyMappings :: GetHypervisorPropertyMappingsResponse -> TestTree
responseGetHypervisorPropertyMappings =
  res
    "GetHypervisorPropertyMappingsResponse"
    "fixture/GetHypervisorPropertyMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHypervisorPropertyMappings)

responseGetVirtualMachine :: GetVirtualMachineResponse -> TestTree
responseGetVirtualMachine =
  res
    "GetVirtualMachineResponse"
    "fixture/GetVirtualMachineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVirtualMachine)

responseImportHypervisorConfiguration :: ImportHypervisorConfigurationResponse -> TestTree
responseImportHypervisorConfiguration =
  res
    "ImportHypervisorConfigurationResponse"
    "fixture/ImportHypervisorConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportHypervisorConfiguration)

responseListGateways :: ListGatewaysResponse -> TestTree
responseListGateways =
  res
    "ListGatewaysResponse"
    "fixture/ListGatewaysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGateways)

responseListHypervisors :: ListHypervisorsResponse -> TestTree
responseListHypervisors =
  res
    "ListHypervisorsResponse"
    "fixture/ListHypervisorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHypervisors)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListVirtualMachines :: ListVirtualMachinesResponse -> TestTree
responseListVirtualMachines =
  res
    "ListVirtualMachinesResponse"
    "fixture/ListVirtualMachinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVirtualMachines)

responsePutBandwidthRateLimitSchedule :: PutBandwidthRateLimitScheduleResponse -> TestTree
responsePutBandwidthRateLimitSchedule =
  res
    "PutBandwidthRateLimitScheduleResponse"
    "fixture/PutBandwidthRateLimitScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBandwidthRateLimitSchedule)

responsePutHypervisorPropertyMappings :: PutHypervisorPropertyMappingsResponse -> TestTree
responsePutHypervisorPropertyMappings =
  res
    "PutHypervisorPropertyMappingsResponse"
    "fixture/PutHypervisorPropertyMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutHypervisorPropertyMappings)

responsePutMaintenanceStartTime :: PutMaintenanceStartTimeResponse -> TestTree
responsePutMaintenanceStartTime =
  res
    "PutMaintenanceStartTimeResponse"
    "fixture/PutMaintenanceStartTimeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMaintenanceStartTime)

responseStartVirtualMachinesMetadataSync :: StartVirtualMachinesMetadataSyncResponse -> TestTree
responseStartVirtualMachinesMetadataSync =
  res
    "StartVirtualMachinesMetadataSyncResponse"
    "fixture/StartVirtualMachinesMetadataSyncResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartVirtualMachinesMetadataSync)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestHypervisorConfiguration :: TestHypervisorConfigurationResponse -> TestTree
responseTestHypervisorConfiguration =
  res
    "TestHypervisorConfigurationResponse"
    "fixture/TestHypervisorConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestHypervisorConfiguration)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateGatewayInformation :: UpdateGatewayInformationResponse -> TestTree
responseUpdateGatewayInformation =
  res
    "UpdateGatewayInformationResponse"
    "fixture/UpdateGatewayInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayInformation)

responseUpdateGatewaySoftwareNow :: UpdateGatewaySoftwareNowResponse -> TestTree
responseUpdateGatewaySoftwareNow =
  res
    "UpdateGatewaySoftwareNowResponse"
    "fixture/UpdateGatewaySoftwareNowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewaySoftwareNow)

responseUpdateHypervisor :: UpdateHypervisorResponse -> TestTree
responseUpdateHypervisor =
  res
    "UpdateHypervisorResponse"
    "fixture/UpdateHypervisorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHypervisor)
