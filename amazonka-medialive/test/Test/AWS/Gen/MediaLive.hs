{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaLive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MediaLive where

import Data.Proxy
import Network.AWS.MediaLive
import Test.AWS.Fixture
import Test.AWS.MediaLive.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListInputDevices $
--             newListInputDevices
--
--         , requestDeleteInput $
--             newDeleteInput
--
--         , requestUpdateInputDevice $
--             newUpdateInputDevice'
--
--         , requestCreateChannel $
--             newCreateChannel'
--
--         , requestUpdateInput $
--             newUpdateInput'
--
--         , requestListInputs $
--             newListInputs
--
--         , requestDescribeInputDeviceThumbnail $
--             newDescribeInputDeviceThumbnail
--
--         , requestBatchStop $
--             newBatchStop'
--
--         , requestUpdateChannelClass $
--             newUpdateChannelClass'
--
--         , requestBatchStart $
--             newBatchStart'
--
--         , requestListInputDeviceTransfers $
--             newListInputDeviceTransfers
--
--         , requestListOfferings $
--             newListOfferings
--
--         , requestDeleteMultiplex $
--             newDeleteMultiplex
--
--         , requestUpdateMultiplex $
--             newUpdateMultiplex'
--
--         , requestUpdateInputSecurityGroup $
--             newUpdateInputSecurityGroup
--
--         , requestListInputSecurityGroups $
--             newListInputSecurityGroups
--
--         , requestDeleteInputSecurityGroup $
--             newDeleteInputSecurityGroup
--
--         , requestStartChannel $
--             newStartChannel
--
--         , requestCreateInputSecurityGroup $
--             newCreateInputSecurityGroup
--
--         , requestDescribeInputDevice $
--             newDescribeInputDevice
--
--         , requestStopChannel $
--             newStopChannel
--
--         , requestDescribeInput $
--             newDescribeInput
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestBatchUpdateSchedule $
--             newBatchUpdateSchedule
--
--         , requestDescribeReservation $
--             newDescribeReservation
--
--         , requestUpdateMultiplexProgram $
--             newUpdateMultiplexProgram'
--
--         , requestAcceptInputDeviceTransfer $
--             newAcceptInputDeviceTransfer
--
--         , requestDeleteMultiplexProgram $
--             newDeleteMultiplexProgram
--
--         , requestDescribeOffering $
--             newDescribeOffering
--
--         , requestRejectInputDeviceTransfer $
--             newRejectInputDeviceTransfer
--
--         , requestDescribeInputSecurityGroup $
--             newDescribeInputSecurityGroup
--
--         , requestListChannels $
--             newListChannels
--
--         , requestDeleteSchedule $
--             newDeleteSchedule
--
--         , requestCreatePartnerInput $
--             newCreatePartnerInput'
--
--         , requestListMultiplexes $
--             newListMultiplexes
--
--         , requestCreateMultiplex $
--             newCreateMultiplex'
--
--         , requestDeleteReservation $
--             newDeleteReservation
--
--         , requestUpdateReservation $
--             newUpdateReservation'
--
--         , requestDescribeMultiplexProgram $
--             newDescribeMultiplexProgram
--
--         , requestListReservations $
--             newListReservations
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestCancelInputDeviceTransfer $
--             newCancelInputDeviceTransfer
--
--         , requestPurchaseOffering $
--             newPurchaseOffering'
--
--         , requestStartMultiplex $
--             newStartMultiplex
--
--         , requestStopMultiplex $
--             newStopMultiplex
--
--         , requestCreateMultiplexProgram $
--             newCreateMultiplexProgram'
--
--         , requestDescribeSchedule $
--             newDescribeSchedule
--
--         , requestDescribeMultiplex $
--             newDescribeMultiplex
--
--         , requestBatchDelete $
--             newBatchDelete'
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestTransferInputDevice $
--             newTransferInputDevice'
--
--         , requestListMultiplexPrograms $
--             newListMultiplexPrograms
--
--         , requestUpdateChannel $
--             newUpdateChannel'
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestCreateInput $
--             newCreateInput'
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseListInputDevices $
--             newListInputDevicesResponse
--
--         , responseDeleteInput $
--             newDeleteInputResponse
--
--         , responseUpdateInputDevice $
--             newUpdateInputDeviceResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseUpdateInput $
--             newUpdateInputResponse
--
--         , responseListInputs $
--             newListInputsResponse
--
--         , responseDescribeInputDeviceThumbnail $
--             newDescribeInputDeviceThumbnailResponse
--
--         , responseBatchStop $
--             newBatchStopResponse
--
--         , responseUpdateChannelClass $
--             newUpdateChannelClassResponse
--
--         , responseBatchStart $
--             newBatchStartResponse
--
--         , responseListInputDeviceTransfers $
--             newListInputDeviceTransfersResponse
--
--         , responseListOfferings $
--             newListOfferingsResponse
--
--         , responseDeleteMultiplex $
--             newDeleteMultiplexResponse
--
--         , responseUpdateMultiplex $
--             newUpdateMultiplexResponse
--
--         , responseUpdateInputSecurityGroup $
--             newUpdateInputSecurityGroupResponse
--
--         , responseListInputSecurityGroups $
--             newListInputSecurityGroupsResponse
--
--         , responseDeleteInputSecurityGroup $
--             newDeleteInputSecurityGroupResponse
--
--         , responseStartChannel $
--             newStartChannelResponse
--
--         , responseCreateInputSecurityGroup $
--             newCreateInputSecurityGroupResponse
--
--         , responseDescribeInputDevice $
--             newDescribeInputDeviceResponse
--
--         , responseStopChannel $
--             newStopChannelResponse
--
--         , responseDescribeInput $
--             newDescribeInputResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseBatchUpdateSchedule $
--             newBatchUpdateScheduleResponse
--
--         , responseDescribeReservation $
--             newDescribeReservationResponse
--
--         , responseUpdateMultiplexProgram $
--             newUpdateMultiplexProgramResponse
--
--         , responseAcceptInputDeviceTransfer $
--             newAcceptInputDeviceTransferResponse
--
--         , responseDeleteMultiplexProgram $
--             newDeleteMultiplexProgramResponse
--
--         , responseDescribeOffering $
--             newDescribeOfferingResponse
--
--         , responseRejectInputDeviceTransfer $
--             newRejectInputDeviceTransferResponse
--
--         , responseDescribeInputSecurityGroup $
--             newDescribeInputSecurityGroupResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseDeleteSchedule $
--             newDeleteScheduleResponse
--
--         , responseCreatePartnerInput $
--             newCreatePartnerInputResponse
--
--         , responseListMultiplexes $
--             newListMultiplexesResponse
--
--         , responseCreateMultiplex $
--             newCreateMultiplexResponse
--
--         , responseDeleteReservation $
--             newDeleteReservationResponse
--
--         , responseUpdateReservation $
--             newUpdateReservationResponse
--
--         , responseDescribeMultiplexProgram $
--             newDescribeMultiplexProgramResponse
--
--         , responseListReservations $
--             newListReservationsResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseCancelInputDeviceTransfer $
--             newCancelInputDeviceTransferResponse
--
--         , responsePurchaseOffering $
--             newPurchaseOfferingResponse
--
--         , responseStartMultiplex $
--             newStartMultiplexResponse
--
--         , responseStopMultiplex $
--             newStopMultiplexResponse
--
--         , responseCreateMultiplexProgram $
--             newCreateMultiplexProgramResponse
--
--         , responseDescribeSchedule $
--             newDescribeScheduleResponse
--
--         , responseDescribeMultiplex $
--             newDescribeMultiplexResponse
--
--         , responseBatchDelete $
--             newBatchDeleteResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseTransferInputDevice $
--             newTransferInputDeviceResponse
--
--         , responseListMultiplexPrograms $
--             newListMultiplexProgramsResponse
--
--         , responseUpdateChannel $
--             newUpdateChannelResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseCreateInput $
--             newCreateInputResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestListInputDevices :: ListInputDevices -> TestTree
requestListInputDevices =
  req
    "ListInputDevices"
    "fixture/ListInputDevices.yaml"

requestDeleteInput :: DeleteInput -> TestTree
requestDeleteInput =
  req
    "DeleteInput"
    "fixture/DeleteInput.yaml"

requestUpdateInputDevice :: UpdateInputDevice' -> TestTree
requestUpdateInputDevice =
  req
    "UpdateInputDevice"
    "fixture/UpdateInputDevice.yaml"

requestCreateChannel :: CreateChannel' -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestUpdateInput :: UpdateInput' -> TestTree
requestUpdateInput =
  req
    "UpdateInput"
    "fixture/UpdateInput.yaml"

requestListInputs :: ListInputs -> TestTree
requestListInputs =
  req
    "ListInputs"
    "fixture/ListInputs.yaml"

requestDescribeInputDeviceThumbnail :: DescribeInputDeviceThumbnail -> TestTree
requestDescribeInputDeviceThumbnail =
  req
    "DescribeInputDeviceThumbnail"
    "fixture/DescribeInputDeviceThumbnail.yaml"

requestBatchStop :: BatchStop' -> TestTree
requestBatchStop =
  req
    "BatchStop"
    "fixture/BatchStop.yaml"

requestUpdateChannelClass :: UpdateChannelClass' -> TestTree
requestUpdateChannelClass =
  req
    "UpdateChannelClass"
    "fixture/UpdateChannelClass.yaml"

requestBatchStart :: BatchStart' -> TestTree
requestBatchStart =
  req
    "BatchStart"
    "fixture/BatchStart.yaml"

requestListInputDeviceTransfers :: ListInputDeviceTransfers -> TestTree
requestListInputDeviceTransfers =
  req
    "ListInputDeviceTransfers"
    "fixture/ListInputDeviceTransfers.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

requestDeleteMultiplex :: DeleteMultiplex -> TestTree
requestDeleteMultiplex =
  req
    "DeleteMultiplex"
    "fixture/DeleteMultiplex.yaml"

requestUpdateMultiplex :: UpdateMultiplex' -> TestTree
requestUpdateMultiplex =
  req
    "UpdateMultiplex"
    "fixture/UpdateMultiplex.yaml"

requestUpdateInputSecurityGroup :: UpdateInputSecurityGroup -> TestTree
requestUpdateInputSecurityGroup =
  req
    "UpdateInputSecurityGroup"
    "fixture/UpdateInputSecurityGroup.yaml"

requestListInputSecurityGroups :: ListInputSecurityGroups -> TestTree
requestListInputSecurityGroups =
  req
    "ListInputSecurityGroups"
    "fixture/ListInputSecurityGroups.yaml"

requestDeleteInputSecurityGroup :: DeleteInputSecurityGroup -> TestTree
requestDeleteInputSecurityGroup =
  req
    "DeleteInputSecurityGroup"
    "fixture/DeleteInputSecurityGroup.yaml"

requestStartChannel :: StartChannel -> TestTree
requestStartChannel =
  req
    "StartChannel"
    "fixture/StartChannel.yaml"

requestCreateInputSecurityGroup :: CreateInputSecurityGroup -> TestTree
requestCreateInputSecurityGroup =
  req
    "CreateInputSecurityGroup"
    "fixture/CreateInputSecurityGroup.yaml"

requestDescribeInputDevice :: DescribeInputDevice -> TestTree
requestDescribeInputDevice =
  req
    "DescribeInputDevice"
    "fixture/DescribeInputDevice.yaml"

requestStopChannel :: StopChannel -> TestTree
requestStopChannel =
  req
    "StopChannel"
    "fixture/StopChannel.yaml"

requestDescribeInput :: DescribeInput -> TestTree
requestDescribeInput =
  req
    "DescribeInput"
    "fixture/DescribeInput.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestBatchUpdateSchedule :: BatchUpdateSchedule -> TestTree
requestBatchUpdateSchedule =
  req
    "BatchUpdateSchedule"
    "fixture/BatchUpdateSchedule.yaml"

requestDescribeReservation :: DescribeReservation -> TestTree
requestDescribeReservation =
  req
    "DescribeReservation"
    "fixture/DescribeReservation.yaml"

requestUpdateMultiplexProgram :: UpdateMultiplexProgram' -> TestTree
requestUpdateMultiplexProgram =
  req
    "UpdateMultiplexProgram"
    "fixture/UpdateMultiplexProgram.yaml"

requestAcceptInputDeviceTransfer :: AcceptInputDeviceTransfer -> TestTree
requestAcceptInputDeviceTransfer =
  req
    "AcceptInputDeviceTransfer"
    "fixture/AcceptInputDeviceTransfer.yaml"

requestDeleteMultiplexProgram :: DeleteMultiplexProgram -> TestTree
requestDeleteMultiplexProgram =
  req
    "DeleteMultiplexProgram"
    "fixture/DeleteMultiplexProgram.yaml"

requestDescribeOffering :: DescribeOffering -> TestTree
requestDescribeOffering =
  req
    "DescribeOffering"
    "fixture/DescribeOffering.yaml"

requestRejectInputDeviceTransfer :: RejectInputDeviceTransfer -> TestTree
requestRejectInputDeviceTransfer =
  req
    "RejectInputDeviceTransfer"
    "fixture/RejectInputDeviceTransfer.yaml"

requestDescribeInputSecurityGroup :: DescribeInputSecurityGroup -> TestTree
requestDescribeInputSecurityGroup =
  req
    "DescribeInputSecurityGroup"
    "fixture/DescribeInputSecurityGroup.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestDeleteSchedule :: DeleteSchedule -> TestTree
requestDeleteSchedule =
  req
    "DeleteSchedule"
    "fixture/DeleteSchedule.yaml"

requestCreatePartnerInput :: CreatePartnerInput' -> TestTree
requestCreatePartnerInput =
  req
    "CreatePartnerInput"
    "fixture/CreatePartnerInput.yaml"

requestListMultiplexes :: ListMultiplexes -> TestTree
requestListMultiplexes =
  req
    "ListMultiplexes"
    "fixture/ListMultiplexes.yaml"

requestCreateMultiplex :: CreateMultiplex' -> TestTree
requestCreateMultiplex =
  req
    "CreateMultiplex"
    "fixture/CreateMultiplex.yaml"

requestDeleteReservation :: DeleteReservation -> TestTree
requestDeleteReservation =
  req
    "DeleteReservation"
    "fixture/DeleteReservation.yaml"

requestUpdateReservation :: UpdateReservation' -> TestTree
requestUpdateReservation =
  req
    "UpdateReservation"
    "fixture/UpdateReservation.yaml"

requestDescribeMultiplexProgram :: DescribeMultiplexProgram -> TestTree
requestDescribeMultiplexProgram =
  req
    "DescribeMultiplexProgram"
    "fixture/DescribeMultiplexProgram.yaml"

requestListReservations :: ListReservations -> TestTree
requestListReservations =
  req
    "ListReservations"
    "fixture/ListReservations.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestCancelInputDeviceTransfer :: CancelInputDeviceTransfer -> TestTree
requestCancelInputDeviceTransfer =
  req
    "CancelInputDeviceTransfer"
    "fixture/CancelInputDeviceTransfer.yaml"

requestPurchaseOffering :: PurchaseOffering' -> TestTree
requestPurchaseOffering =
  req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestStartMultiplex :: StartMultiplex -> TestTree
requestStartMultiplex =
  req
    "StartMultiplex"
    "fixture/StartMultiplex.yaml"

requestStopMultiplex :: StopMultiplex -> TestTree
requestStopMultiplex =
  req
    "StopMultiplex"
    "fixture/StopMultiplex.yaml"

requestCreateMultiplexProgram :: CreateMultiplexProgram' -> TestTree
requestCreateMultiplexProgram =
  req
    "CreateMultiplexProgram"
    "fixture/CreateMultiplexProgram.yaml"

requestDescribeSchedule :: DescribeSchedule -> TestTree
requestDescribeSchedule =
  req
    "DescribeSchedule"
    "fixture/DescribeSchedule.yaml"

requestDescribeMultiplex :: DescribeMultiplex -> TestTree
requestDescribeMultiplex =
  req
    "DescribeMultiplex"
    "fixture/DescribeMultiplex.yaml"

requestBatchDelete :: BatchDelete' -> TestTree
requestBatchDelete =
  req
    "BatchDelete"
    "fixture/BatchDelete.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestTransferInputDevice :: TransferInputDevice' -> TestTree
requestTransferInputDevice =
  req
    "TransferInputDevice"
    "fixture/TransferInputDevice.yaml"

requestListMultiplexPrograms :: ListMultiplexPrograms -> TestTree
requestListMultiplexPrograms =
  req
    "ListMultiplexPrograms"
    "fixture/ListMultiplexPrograms.yaml"

requestUpdateChannel :: UpdateChannel' -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestCreateInput :: CreateInput' -> TestTree
requestCreateInput =
  req
    "CreateInput"
    "fixture/CreateInput.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseListInputDevices :: ListInputDevicesResponse -> TestTree
responseListInputDevices =
  res
    "ListInputDevicesResponse"
    "fixture/ListInputDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInputDevices)

responseDeleteInput :: DeleteInputResponse -> TestTree
responseDeleteInput =
  res
    "DeleteInputResponse"
    "fixture/DeleteInputResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInput)

responseUpdateInputDevice :: UpdateInputDeviceResponse -> TestTree
responseUpdateInputDevice =
  res
    "UpdateInputDeviceResponse"
    "fixture/UpdateInputDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInputDevice')

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChannel')

responseUpdateInput :: UpdateInputResponse -> TestTree
responseUpdateInput =
  res
    "UpdateInputResponse"
    "fixture/UpdateInputResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInput')

responseListInputs :: ListInputsResponse -> TestTree
responseListInputs =
  res
    "ListInputsResponse"
    "fixture/ListInputsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInputs)

responseBatchStop :: BatchStopResponse -> TestTree
responseBatchStop =
  res
    "BatchStopResponse"
    "fixture/BatchStopResponse.proto"
    defaultService
    (Proxy :: Proxy BatchStop')

responseUpdateChannelClass :: UpdateChannelClassResponse -> TestTree
responseUpdateChannelClass =
  res
    "UpdateChannelClassResponse"
    "fixture/UpdateChannelClassResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateChannelClass')

responseBatchStart :: BatchStartResponse -> TestTree
responseBatchStart =
  res
    "BatchStartResponse"
    "fixture/BatchStartResponse.proto"
    defaultService
    (Proxy :: Proxy BatchStart')

responseListInputDeviceTransfers :: ListInputDeviceTransfersResponse -> TestTree
responseListInputDeviceTransfers =
  res
    "ListInputDeviceTransfersResponse"
    "fixture/ListInputDeviceTransfersResponse.proto"
    defaultService
    (Proxy :: Proxy ListInputDeviceTransfers)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOfferings)

responseDeleteMultiplex :: DeleteMultiplexResponse -> TestTree
responseDeleteMultiplex =
  res
    "DeleteMultiplexResponse"
    "fixture/DeleteMultiplexResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMultiplex)

responseUpdateMultiplex :: UpdateMultiplexResponse -> TestTree
responseUpdateMultiplex =
  res
    "UpdateMultiplexResponse"
    "fixture/UpdateMultiplexResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMultiplex')

responseUpdateInputSecurityGroup :: UpdateInputSecurityGroupResponse -> TestTree
responseUpdateInputSecurityGroup =
  res
    "UpdateInputSecurityGroupResponse"
    "fixture/UpdateInputSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInputSecurityGroup)

responseListInputSecurityGroups :: ListInputSecurityGroupsResponse -> TestTree
responseListInputSecurityGroups =
  res
    "ListInputSecurityGroupsResponse"
    "fixture/ListInputSecurityGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInputSecurityGroups)

responseDeleteInputSecurityGroup :: DeleteInputSecurityGroupResponse -> TestTree
responseDeleteInputSecurityGroup =
  res
    "DeleteInputSecurityGroupResponse"
    "fixture/DeleteInputSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInputSecurityGroup)

responseStartChannel :: StartChannelResponse -> TestTree
responseStartChannel =
  res
    "StartChannelResponse"
    "fixture/StartChannelResponse.proto"
    defaultService
    (Proxy :: Proxy StartChannel)

responseCreateInputSecurityGroup :: CreateInputSecurityGroupResponse -> TestTree
responseCreateInputSecurityGroup =
  res
    "CreateInputSecurityGroupResponse"
    "fixture/CreateInputSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInputSecurityGroup)

responseDescribeInputDevice :: DescribeInputDeviceResponse -> TestTree
responseDescribeInputDevice =
  res
    "DescribeInputDeviceResponse"
    "fixture/DescribeInputDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInputDevice)

responseStopChannel :: StopChannelResponse -> TestTree
responseStopChannel =
  res
    "StopChannelResponse"
    "fixture/StopChannelResponse.proto"
    defaultService
    (Proxy :: Proxy StopChannel)

responseDescribeInput :: DescribeInputResponse -> TestTree
responseDescribeInput =
  res
    "DescribeInputResponse"
    "fixture/DescribeInputResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInput)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseBatchUpdateSchedule :: BatchUpdateScheduleResponse -> TestTree
responseBatchUpdateSchedule =
  res
    "BatchUpdateScheduleResponse"
    "fixture/BatchUpdateScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy BatchUpdateSchedule)

responseDescribeReservation :: DescribeReservationResponse -> TestTree
responseDescribeReservation =
  res
    "DescribeReservationResponse"
    "fixture/DescribeReservationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservation)

responseUpdateMultiplexProgram :: UpdateMultiplexProgramResponse -> TestTree
responseUpdateMultiplexProgram =
  res
    "UpdateMultiplexProgramResponse"
    "fixture/UpdateMultiplexProgramResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMultiplexProgram')

responseAcceptInputDeviceTransfer :: AcceptInputDeviceTransferResponse -> TestTree
responseAcceptInputDeviceTransfer =
  res
    "AcceptInputDeviceTransferResponse"
    "fixture/AcceptInputDeviceTransferResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptInputDeviceTransfer)

responseDeleteMultiplexProgram :: DeleteMultiplexProgramResponse -> TestTree
responseDeleteMultiplexProgram =
  res
    "DeleteMultiplexProgramResponse"
    "fixture/DeleteMultiplexProgramResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMultiplexProgram)

responseDescribeOffering :: DescribeOfferingResponse -> TestTree
responseDescribeOffering =
  res
    "DescribeOfferingResponse"
    "fixture/DescribeOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOffering)

responseRejectInputDeviceTransfer :: RejectInputDeviceTransferResponse -> TestTree
responseRejectInputDeviceTransfer =
  res
    "RejectInputDeviceTransferResponse"
    "fixture/RejectInputDeviceTransferResponse.proto"
    defaultService
    (Proxy :: Proxy RejectInputDeviceTransfer)

responseDescribeInputSecurityGroup :: DescribeInputSecurityGroupResponse -> TestTree
responseDescribeInputSecurityGroup =
  res
    "DescribeInputSecurityGroupResponse"
    "fixture/DescribeInputSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInputSecurityGroup)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannels)

responseDeleteSchedule :: DeleteScheduleResponse -> TestTree
responseDeleteSchedule =
  res
    "DeleteScheduleResponse"
    "fixture/DeleteScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSchedule)

responseCreatePartnerInput :: CreatePartnerInputResponse -> TestTree
responseCreatePartnerInput =
  res
    "CreatePartnerInputResponse"
    "fixture/CreatePartnerInputResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePartnerInput')

responseListMultiplexes :: ListMultiplexesResponse -> TestTree
responseListMultiplexes =
  res
    "ListMultiplexesResponse"
    "fixture/ListMultiplexesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMultiplexes)

responseCreateMultiplex :: CreateMultiplexResponse -> TestTree
responseCreateMultiplex =
  res
    "CreateMultiplexResponse"
    "fixture/CreateMultiplexResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMultiplex')

responseDeleteReservation :: DeleteReservationResponse -> TestTree
responseDeleteReservation =
  res
    "DeleteReservationResponse"
    "fixture/DeleteReservationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReservation)

responseUpdateReservation :: UpdateReservationResponse -> TestTree
responseUpdateReservation =
  res
    "UpdateReservationResponse"
    "fixture/UpdateReservationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateReservation')

responseDescribeMultiplexProgram :: DescribeMultiplexProgramResponse -> TestTree
responseDescribeMultiplexProgram =
  res
    "DescribeMultiplexProgramResponse"
    "fixture/DescribeMultiplexProgramResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMultiplexProgram)

responseListReservations :: ListReservationsResponse -> TestTree
responseListReservations =
  res
    "ListReservationsResponse"
    "fixture/ListReservationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListReservations)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChannel)

responseCancelInputDeviceTransfer :: CancelInputDeviceTransferResponse -> TestTree
responseCancelInputDeviceTransfer =
  res
    "CancelInputDeviceTransferResponse"
    "fixture/CancelInputDeviceTransferResponse.proto"
    defaultService
    (Proxy :: Proxy CancelInputDeviceTransfer)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseOffering')

responseStartMultiplex :: StartMultiplexResponse -> TestTree
responseStartMultiplex =
  res
    "StartMultiplexResponse"
    "fixture/StartMultiplexResponse.proto"
    defaultService
    (Proxy :: Proxy StartMultiplex)

responseStopMultiplex :: StopMultiplexResponse -> TestTree
responseStopMultiplex =
  res
    "StopMultiplexResponse"
    "fixture/StopMultiplexResponse.proto"
    defaultService
    (Proxy :: Proxy StopMultiplex)

responseCreateMultiplexProgram :: CreateMultiplexProgramResponse -> TestTree
responseCreateMultiplexProgram =
  res
    "CreateMultiplexProgramResponse"
    "fixture/CreateMultiplexProgramResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMultiplexProgram')

responseDescribeSchedule :: DescribeScheduleResponse -> TestTree
responseDescribeSchedule =
  res
    "DescribeScheduleResponse"
    "fixture/DescribeScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSchedule)

responseDescribeMultiplex :: DescribeMultiplexResponse -> TestTree
responseDescribeMultiplex =
  res
    "DescribeMultiplexResponse"
    "fixture/DescribeMultiplexResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMultiplex)

responseBatchDelete :: BatchDeleteResponse -> TestTree
responseBatchDelete =
  res
    "BatchDeleteResponse"
    "fixture/BatchDeleteResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDelete')

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTags)

responseTransferInputDevice :: TransferInputDeviceResponse -> TestTree
responseTransferInputDevice =
  res
    "TransferInputDeviceResponse"
    "fixture/TransferInputDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy TransferInputDevice')

responseListMultiplexPrograms :: ListMultiplexProgramsResponse -> TestTree
responseListMultiplexPrograms =
  res
    "ListMultiplexProgramsResponse"
    "fixture/ListMultiplexProgramsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMultiplexPrograms)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateChannel')

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChannel)

responseCreateInput :: CreateInputResponse -> TestTree
responseCreateInput =
  res
    "CreateInputResponse"
    "fixture/CreateInputResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInput')

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
