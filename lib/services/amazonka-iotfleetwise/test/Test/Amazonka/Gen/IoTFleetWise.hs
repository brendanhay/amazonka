{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTFleetWise
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTFleetWise where

import Amazonka.IoTFleetWise
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTFleetWise.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateVehicleFleet $
--             newAssociateVehicleFleet
--
--         , requestBatchCreateVehicle $
--             newBatchCreateVehicle
--
--         , requestBatchUpdateVehicle $
--             newBatchUpdateVehicle
--
--         , requestCreateCampaign $
--             newCreateCampaign
--
--         , requestCreateDecoderManifest $
--             newCreateDecoderManifest
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestCreateModelManifest $
--             newCreateModelManifest
--
--         , requestCreateSignalCatalog $
--             newCreateSignalCatalog
--
--         , requestCreateVehicle $
--             newCreateVehicle
--
--         , requestDeleteCampaign $
--             newDeleteCampaign
--
--         , requestDeleteDecoderManifest $
--             newDeleteDecoderManifest
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestDeleteModelManifest $
--             newDeleteModelManifest
--
--         , requestDeleteSignalCatalog $
--             newDeleteSignalCatalog
--
--         , requestDeleteVehicle $
--             newDeleteVehicle
--
--         , requestDisassociateVehicleFleet $
--             newDisassociateVehicleFleet
--
--         , requestGetCampaign $
--             newGetCampaign
--
--         , requestGetDecoderManifest $
--             newGetDecoderManifest
--
--         , requestGetFleet $
--             newGetFleet
--
--         , requestGetLoggingOptions $
--             newGetLoggingOptions
--
--         , requestGetModelManifest $
--             newGetModelManifest
--
--         , requestGetRegisterAccountStatus $
--             newGetRegisterAccountStatus
--
--         , requestGetSignalCatalog $
--             newGetSignalCatalog
--
--         , requestGetVehicle $
--             newGetVehicle
--
--         , requestGetVehicleStatus $
--             newGetVehicleStatus
--
--         , requestImportDecoderManifest $
--             newImportDecoderManifest
--
--         , requestImportSignalCatalog $
--             newImportSignalCatalog
--
--         , requestListCampaigns $
--             newListCampaigns
--
--         , requestListDecoderManifestNetworkInterfaces $
--             newListDecoderManifestNetworkInterfaces
--
--         , requestListDecoderManifestSignals $
--             newListDecoderManifestSignals
--
--         , requestListDecoderManifests $
--             newListDecoderManifests
--
--         , requestListFleets $
--             newListFleets
--
--         , requestListFleetsForVehicle $
--             newListFleetsForVehicle
--
--         , requestListModelManifestNodes $
--             newListModelManifestNodes
--
--         , requestListModelManifests $
--             newListModelManifests
--
--         , requestListSignalCatalogNodes $
--             newListSignalCatalogNodes
--
--         , requestListSignalCatalogs $
--             newListSignalCatalogs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListVehicles $
--             newListVehicles
--
--         , requestListVehiclesInFleet $
--             newListVehiclesInFleet
--
--         , requestPutLoggingOptions $
--             newPutLoggingOptions
--
--         , requestRegisterAccount $
--             newRegisterAccount
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCampaign $
--             newUpdateCampaign
--
--         , requestUpdateDecoderManifest $
--             newUpdateDecoderManifest
--
--         , requestUpdateFleet $
--             newUpdateFleet
--
--         , requestUpdateModelManifest $
--             newUpdateModelManifest
--
--         , requestUpdateSignalCatalog $
--             newUpdateSignalCatalog
--
--         , requestUpdateVehicle $
--             newUpdateVehicle
--
--           ]

--     , testGroup "response"
--         [ responseAssociateVehicleFleet $
--             newAssociateVehicleFleetResponse
--
--         , responseBatchCreateVehicle $
--             newBatchCreateVehicleResponse
--
--         , responseBatchUpdateVehicle $
--             newBatchUpdateVehicleResponse
--
--         , responseCreateCampaign $
--             newCreateCampaignResponse
--
--         , responseCreateDecoderManifest $
--             newCreateDecoderManifestResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseCreateModelManifest $
--             newCreateModelManifestResponse
--
--         , responseCreateSignalCatalog $
--             newCreateSignalCatalogResponse
--
--         , responseCreateVehicle $
--             newCreateVehicleResponse
--
--         , responseDeleteCampaign $
--             newDeleteCampaignResponse
--
--         , responseDeleteDecoderManifest $
--             newDeleteDecoderManifestResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseDeleteModelManifest $
--             newDeleteModelManifestResponse
--
--         , responseDeleteSignalCatalog $
--             newDeleteSignalCatalogResponse
--
--         , responseDeleteVehicle $
--             newDeleteVehicleResponse
--
--         , responseDisassociateVehicleFleet $
--             newDisassociateVehicleFleetResponse
--
--         , responseGetCampaign $
--             newGetCampaignResponse
--
--         , responseGetDecoderManifest $
--             newGetDecoderManifestResponse
--
--         , responseGetFleet $
--             newGetFleetResponse
--
--         , responseGetLoggingOptions $
--             newGetLoggingOptionsResponse
--
--         , responseGetModelManifest $
--             newGetModelManifestResponse
--
--         , responseGetRegisterAccountStatus $
--             newGetRegisterAccountStatusResponse
--
--         , responseGetSignalCatalog $
--             newGetSignalCatalogResponse
--
--         , responseGetVehicle $
--             newGetVehicleResponse
--
--         , responseGetVehicleStatus $
--             newGetVehicleStatusResponse
--
--         , responseImportDecoderManifest $
--             newImportDecoderManifestResponse
--
--         , responseImportSignalCatalog $
--             newImportSignalCatalogResponse
--
--         , responseListCampaigns $
--             newListCampaignsResponse
--
--         , responseListDecoderManifestNetworkInterfaces $
--             newListDecoderManifestNetworkInterfacesResponse
--
--         , responseListDecoderManifestSignals $
--             newListDecoderManifestSignalsResponse
--
--         , responseListDecoderManifests $
--             newListDecoderManifestsResponse
--
--         , responseListFleets $
--             newListFleetsResponse
--
--         , responseListFleetsForVehicle $
--             newListFleetsForVehicleResponse
--
--         , responseListModelManifestNodes $
--             newListModelManifestNodesResponse
--
--         , responseListModelManifests $
--             newListModelManifestsResponse
--
--         , responseListSignalCatalogNodes $
--             newListSignalCatalogNodesResponse
--
--         , responseListSignalCatalogs $
--             newListSignalCatalogsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListVehicles $
--             newListVehiclesResponse
--
--         , responseListVehiclesInFleet $
--             newListVehiclesInFleetResponse
--
--         , responsePutLoggingOptions $
--             newPutLoggingOptionsResponse
--
--         , responseRegisterAccount $
--             newRegisterAccountResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCampaign $
--             newUpdateCampaignResponse
--
--         , responseUpdateDecoderManifest $
--             newUpdateDecoderManifestResponse
--
--         , responseUpdateFleet $
--             newUpdateFleetResponse
--
--         , responseUpdateModelManifest $
--             newUpdateModelManifestResponse
--
--         , responseUpdateSignalCatalog $
--             newUpdateSignalCatalogResponse
--
--         , responseUpdateVehicle $
--             newUpdateVehicleResponse
--
--           ]
--     ]

-- Requests

requestAssociateVehicleFleet :: AssociateVehicleFleet -> TestTree
requestAssociateVehicleFleet =
  req
    "AssociateVehicleFleet"
    "fixture/AssociateVehicleFleet.yaml"

requestBatchCreateVehicle :: BatchCreateVehicle -> TestTree
requestBatchCreateVehicle =
  req
    "BatchCreateVehicle"
    "fixture/BatchCreateVehicle.yaml"

requestBatchUpdateVehicle :: BatchUpdateVehicle -> TestTree
requestBatchUpdateVehicle =
  req
    "BatchUpdateVehicle"
    "fixture/BatchUpdateVehicle.yaml"

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign =
  req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestCreateDecoderManifest :: CreateDecoderManifest -> TestTree
requestCreateDecoderManifest =
  req
    "CreateDecoderManifest"
    "fixture/CreateDecoderManifest.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestCreateModelManifest :: CreateModelManifest -> TestTree
requestCreateModelManifest =
  req
    "CreateModelManifest"
    "fixture/CreateModelManifest.yaml"

requestCreateSignalCatalog :: CreateSignalCatalog -> TestTree
requestCreateSignalCatalog =
  req
    "CreateSignalCatalog"
    "fixture/CreateSignalCatalog.yaml"

requestCreateVehicle :: CreateVehicle -> TestTree
requestCreateVehicle =
  req
    "CreateVehicle"
    "fixture/CreateVehicle.yaml"

requestDeleteCampaign :: DeleteCampaign -> TestTree
requestDeleteCampaign =
  req
    "DeleteCampaign"
    "fixture/DeleteCampaign.yaml"

requestDeleteDecoderManifest :: DeleteDecoderManifest -> TestTree
requestDeleteDecoderManifest =
  req
    "DeleteDecoderManifest"
    "fixture/DeleteDecoderManifest.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestDeleteModelManifest :: DeleteModelManifest -> TestTree
requestDeleteModelManifest =
  req
    "DeleteModelManifest"
    "fixture/DeleteModelManifest.yaml"

requestDeleteSignalCatalog :: DeleteSignalCatalog -> TestTree
requestDeleteSignalCatalog =
  req
    "DeleteSignalCatalog"
    "fixture/DeleteSignalCatalog.yaml"

requestDeleteVehicle :: DeleteVehicle -> TestTree
requestDeleteVehicle =
  req
    "DeleteVehicle"
    "fixture/DeleteVehicle.yaml"

requestDisassociateVehicleFleet :: DisassociateVehicleFleet -> TestTree
requestDisassociateVehicleFleet =
  req
    "DisassociateVehicleFleet"
    "fixture/DisassociateVehicleFleet.yaml"

requestGetCampaign :: GetCampaign -> TestTree
requestGetCampaign =
  req
    "GetCampaign"
    "fixture/GetCampaign.yaml"

requestGetDecoderManifest :: GetDecoderManifest -> TestTree
requestGetDecoderManifest =
  req
    "GetDecoderManifest"
    "fixture/GetDecoderManifest.yaml"

requestGetFleet :: GetFleet -> TestTree
requestGetFleet =
  req
    "GetFleet"
    "fixture/GetFleet.yaml"

requestGetLoggingOptions :: GetLoggingOptions -> TestTree
requestGetLoggingOptions =
  req
    "GetLoggingOptions"
    "fixture/GetLoggingOptions.yaml"

requestGetModelManifest :: GetModelManifest -> TestTree
requestGetModelManifest =
  req
    "GetModelManifest"
    "fixture/GetModelManifest.yaml"

requestGetRegisterAccountStatus :: GetRegisterAccountStatus -> TestTree
requestGetRegisterAccountStatus =
  req
    "GetRegisterAccountStatus"
    "fixture/GetRegisterAccountStatus.yaml"

requestGetSignalCatalog :: GetSignalCatalog -> TestTree
requestGetSignalCatalog =
  req
    "GetSignalCatalog"
    "fixture/GetSignalCatalog.yaml"

requestGetVehicle :: GetVehicle -> TestTree
requestGetVehicle =
  req
    "GetVehicle"
    "fixture/GetVehicle.yaml"

requestGetVehicleStatus :: GetVehicleStatus -> TestTree
requestGetVehicleStatus =
  req
    "GetVehicleStatus"
    "fixture/GetVehicleStatus.yaml"

requestImportDecoderManifest :: ImportDecoderManifest -> TestTree
requestImportDecoderManifest =
  req
    "ImportDecoderManifest"
    "fixture/ImportDecoderManifest.yaml"

requestImportSignalCatalog :: ImportSignalCatalog -> TestTree
requestImportSignalCatalog =
  req
    "ImportSignalCatalog"
    "fixture/ImportSignalCatalog.yaml"

requestListCampaigns :: ListCampaigns -> TestTree
requestListCampaigns =
  req
    "ListCampaigns"
    "fixture/ListCampaigns.yaml"

requestListDecoderManifestNetworkInterfaces :: ListDecoderManifestNetworkInterfaces -> TestTree
requestListDecoderManifestNetworkInterfaces =
  req
    "ListDecoderManifestNetworkInterfaces"
    "fixture/ListDecoderManifestNetworkInterfaces.yaml"

requestListDecoderManifestSignals :: ListDecoderManifestSignals -> TestTree
requestListDecoderManifestSignals =
  req
    "ListDecoderManifestSignals"
    "fixture/ListDecoderManifestSignals.yaml"

requestListDecoderManifests :: ListDecoderManifests -> TestTree
requestListDecoderManifests =
  req
    "ListDecoderManifests"
    "fixture/ListDecoderManifests.yaml"

requestListFleets :: ListFleets -> TestTree
requestListFleets =
  req
    "ListFleets"
    "fixture/ListFleets.yaml"

requestListFleetsForVehicle :: ListFleetsForVehicle -> TestTree
requestListFleetsForVehicle =
  req
    "ListFleetsForVehicle"
    "fixture/ListFleetsForVehicle.yaml"

requestListModelManifestNodes :: ListModelManifestNodes -> TestTree
requestListModelManifestNodes =
  req
    "ListModelManifestNodes"
    "fixture/ListModelManifestNodes.yaml"

requestListModelManifests :: ListModelManifests -> TestTree
requestListModelManifests =
  req
    "ListModelManifests"
    "fixture/ListModelManifests.yaml"

requestListSignalCatalogNodes :: ListSignalCatalogNodes -> TestTree
requestListSignalCatalogNodes =
  req
    "ListSignalCatalogNodes"
    "fixture/ListSignalCatalogNodes.yaml"

requestListSignalCatalogs :: ListSignalCatalogs -> TestTree
requestListSignalCatalogs =
  req
    "ListSignalCatalogs"
    "fixture/ListSignalCatalogs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListVehicles :: ListVehicles -> TestTree
requestListVehicles =
  req
    "ListVehicles"
    "fixture/ListVehicles.yaml"

requestListVehiclesInFleet :: ListVehiclesInFleet -> TestTree
requestListVehiclesInFleet =
  req
    "ListVehiclesInFleet"
    "fixture/ListVehiclesInFleet.yaml"

requestPutLoggingOptions :: PutLoggingOptions -> TestTree
requestPutLoggingOptions =
  req
    "PutLoggingOptions"
    "fixture/PutLoggingOptions.yaml"

requestRegisterAccount :: RegisterAccount -> TestTree
requestRegisterAccount =
  req
    "RegisterAccount"
    "fixture/RegisterAccount.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateCampaign :: UpdateCampaign -> TestTree
requestUpdateCampaign =
  req
    "UpdateCampaign"
    "fixture/UpdateCampaign.yaml"

requestUpdateDecoderManifest :: UpdateDecoderManifest -> TestTree
requestUpdateDecoderManifest =
  req
    "UpdateDecoderManifest"
    "fixture/UpdateDecoderManifest.yaml"

requestUpdateFleet :: UpdateFleet -> TestTree
requestUpdateFleet =
  req
    "UpdateFleet"
    "fixture/UpdateFleet.yaml"

requestUpdateModelManifest :: UpdateModelManifest -> TestTree
requestUpdateModelManifest =
  req
    "UpdateModelManifest"
    "fixture/UpdateModelManifest.yaml"

requestUpdateSignalCatalog :: UpdateSignalCatalog -> TestTree
requestUpdateSignalCatalog =
  req
    "UpdateSignalCatalog"
    "fixture/UpdateSignalCatalog.yaml"

requestUpdateVehicle :: UpdateVehicle -> TestTree
requestUpdateVehicle =
  req
    "UpdateVehicle"
    "fixture/UpdateVehicle.yaml"

-- Responses

responseAssociateVehicleFleet :: AssociateVehicleFleetResponse -> TestTree
responseAssociateVehicleFleet =
  res
    "AssociateVehicleFleetResponse"
    "fixture/AssociateVehicleFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateVehicleFleet)

responseBatchCreateVehicle :: BatchCreateVehicleResponse -> TestTree
responseBatchCreateVehicle =
  res
    "BatchCreateVehicleResponse"
    "fixture/BatchCreateVehicleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateVehicle)

responseBatchUpdateVehicle :: BatchUpdateVehicleResponse -> TestTree
responseBatchUpdateVehicle =
  res
    "BatchUpdateVehicleResponse"
    "fixture/BatchUpdateVehicleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateVehicle)

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCampaign)

responseCreateDecoderManifest :: CreateDecoderManifestResponse -> TestTree
responseCreateDecoderManifest =
  res
    "CreateDecoderManifestResponse"
    "fixture/CreateDecoderManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDecoderManifest)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseCreateModelManifest :: CreateModelManifestResponse -> TestTree
responseCreateModelManifest =
  res
    "CreateModelManifestResponse"
    "fixture/CreateModelManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelManifest)

responseCreateSignalCatalog :: CreateSignalCatalogResponse -> TestTree
responseCreateSignalCatalog =
  res
    "CreateSignalCatalogResponse"
    "fixture/CreateSignalCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSignalCatalog)

responseCreateVehicle :: CreateVehicleResponse -> TestTree
responseCreateVehicle =
  res
    "CreateVehicleResponse"
    "fixture/CreateVehicleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVehicle)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCampaign)

responseDeleteDecoderManifest :: DeleteDecoderManifestResponse -> TestTree
responseDeleteDecoderManifest =
  res
    "DeleteDecoderManifestResponse"
    "fixture/DeleteDecoderManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDecoderManifest)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleet)

responseDeleteModelManifest :: DeleteModelManifestResponse -> TestTree
responseDeleteModelManifest =
  res
    "DeleteModelManifestResponse"
    "fixture/DeleteModelManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelManifest)

responseDeleteSignalCatalog :: DeleteSignalCatalogResponse -> TestTree
responseDeleteSignalCatalog =
  res
    "DeleteSignalCatalogResponse"
    "fixture/DeleteSignalCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSignalCatalog)

responseDeleteVehicle :: DeleteVehicleResponse -> TestTree
responseDeleteVehicle =
  res
    "DeleteVehicleResponse"
    "fixture/DeleteVehicleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVehicle)

responseDisassociateVehicleFleet :: DisassociateVehicleFleetResponse -> TestTree
responseDisassociateVehicleFleet =
  res
    "DisassociateVehicleFleetResponse"
    "fixture/DisassociateVehicleFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateVehicleFleet)

responseGetCampaign :: GetCampaignResponse -> TestTree
responseGetCampaign =
  res
    "GetCampaignResponse"
    "fixture/GetCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaign)

responseGetDecoderManifest :: GetDecoderManifestResponse -> TestTree
responseGetDecoderManifest =
  res
    "GetDecoderManifestResponse"
    "fixture/GetDecoderManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDecoderManifest)

responseGetFleet :: GetFleetResponse -> TestTree
responseGetFleet =
  res
    "GetFleetResponse"
    "fixture/GetFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFleet)

responseGetLoggingOptions :: GetLoggingOptionsResponse -> TestTree
responseGetLoggingOptions =
  res
    "GetLoggingOptionsResponse"
    "fixture/GetLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggingOptions)

responseGetModelManifest :: GetModelManifestResponse -> TestTree
responseGetModelManifest =
  res
    "GetModelManifestResponse"
    "fixture/GetModelManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModelManifest)

responseGetRegisterAccountStatus :: GetRegisterAccountStatusResponse -> TestTree
responseGetRegisterAccountStatus =
  res
    "GetRegisterAccountStatusResponse"
    "fixture/GetRegisterAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegisterAccountStatus)

responseGetSignalCatalog :: GetSignalCatalogResponse -> TestTree
responseGetSignalCatalog =
  res
    "GetSignalCatalogResponse"
    "fixture/GetSignalCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSignalCatalog)

responseGetVehicle :: GetVehicleResponse -> TestTree
responseGetVehicle =
  res
    "GetVehicleResponse"
    "fixture/GetVehicleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVehicle)

responseGetVehicleStatus :: GetVehicleStatusResponse -> TestTree
responseGetVehicleStatus =
  res
    "GetVehicleStatusResponse"
    "fixture/GetVehicleStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVehicleStatus)

responseImportDecoderManifest :: ImportDecoderManifestResponse -> TestTree
responseImportDecoderManifest =
  res
    "ImportDecoderManifestResponse"
    "fixture/ImportDecoderManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportDecoderManifest)

responseImportSignalCatalog :: ImportSignalCatalogResponse -> TestTree
responseImportSignalCatalog =
  res
    "ImportSignalCatalogResponse"
    "fixture/ImportSignalCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportSignalCatalog)

responseListCampaigns :: ListCampaignsResponse -> TestTree
responseListCampaigns =
  res
    "ListCampaignsResponse"
    "fixture/ListCampaignsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCampaigns)

responseListDecoderManifestNetworkInterfaces :: ListDecoderManifestNetworkInterfacesResponse -> TestTree
responseListDecoderManifestNetworkInterfaces =
  res
    "ListDecoderManifestNetworkInterfacesResponse"
    "fixture/ListDecoderManifestNetworkInterfacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDecoderManifestNetworkInterfaces)

responseListDecoderManifestSignals :: ListDecoderManifestSignalsResponse -> TestTree
responseListDecoderManifestSignals =
  res
    "ListDecoderManifestSignalsResponse"
    "fixture/ListDecoderManifestSignalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDecoderManifestSignals)

responseListDecoderManifests :: ListDecoderManifestsResponse -> TestTree
responseListDecoderManifests =
  res
    "ListDecoderManifestsResponse"
    "fixture/ListDecoderManifestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDecoderManifests)

responseListFleets :: ListFleetsResponse -> TestTree
responseListFleets =
  res
    "ListFleetsResponse"
    "fixture/ListFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleets)

responseListFleetsForVehicle :: ListFleetsForVehicleResponse -> TestTree
responseListFleetsForVehicle =
  res
    "ListFleetsForVehicleResponse"
    "fixture/ListFleetsForVehicleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFleetsForVehicle)

responseListModelManifestNodes :: ListModelManifestNodesResponse -> TestTree
responseListModelManifestNodes =
  res
    "ListModelManifestNodesResponse"
    "fixture/ListModelManifestNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelManifestNodes)

responseListModelManifests :: ListModelManifestsResponse -> TestTree
responseListModelManifests =
  res
    "ListModelManifestsResponse"
    "fixture/ListModelManifestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelManifests)

responseListSignalCatalogNodes :: ListSignalCatalogNodesResponse -> TestTree
responseListSignalCatalogNodes =
  res
    "ListSignalCatalogNodesResponse"
    "fixture/ListSignalCatalogNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSignalCatalogNodes)

responseListSignalCatalogs :: ListSignalCatalogsResponse -> TestTree
responseListSignalCatalogs =
  res
    "ListSignalCatalogsResponse"
    "fixture/ListSignalCatalogsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSignalCatalogs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListVehicles :: ListVehiclesResponse -> TestTree
responseListVehicles =
  res
    "ListVehiclesResponse"
    "fixture/ListVehiclesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVehicles)

responseListVehiclesInFleet :: ListVehiclesInFleetResponse -> TestTree
responseListVehiclesInFleet =
  res
    "ListVehiclesInFleetResponse"
    "fixture/ListVehiclesInFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVehiclesInFleet)

responsePutLoggingOptions :: PutLoggingOptionsResponse -> TestTree
responsePutLoggingOptions =
  res
    "PutLoggingOptionsResponse"
    "fixture/PutLoggingOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLoggingOptions)

responseRegisterAccount :: RegisterAccountResponse -> TestTree
responseRegisterAccount =
  res
    "RegisterAccountResponse"
    "fixture/RegisterAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterAccount)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateCampaign :: UpdateCampaignResponse -> TestTree
responseUpdateCampaign =
  res
    "UpdateCampaignResponse"
    "fixture/UpdateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCampaign)

responseUpdateDecoderManifest :: UpdateDecoderManifestResponse -> TestTree
responseUpdateDecoderManifest =
  res
    "UpdateDecoderManifestResponse"
    "fixture/UpdateDecoderManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDecoderManifest)

responseUpdateFleet :: UpdateFleetResponse -> TestTree
responseUpdateFleet =
  res
    "UpdateFleetResponse"
    "fixture/UpdateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleet)

responseUpdateModelManifest :: UpdateModelManifestResponse -> TestTree
responseUpdateModelManifest =
  res
    "UpdateModelManifestResponse"
    "fixture/UpdateModelManifestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModelManifest)

responseUpdateSignalCatalog :: UpdateSignalCatalogResponse -> TestTree
responseUpdateSignalCatalog =
  res
    "UpdateSignalCatalogResponse"
    "fixture/UpdateSignalCatalogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSignalCatalog)

responseUpdateVehicle :: UpdateVehicleResponse -> TestTree
responseUpdateVehicle =
  res
    "UpdateVehicleResponse"
    "fixture/UpdateVehicleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVehicle)
