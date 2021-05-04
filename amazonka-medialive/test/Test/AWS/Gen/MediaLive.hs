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
--         [ requestDescribeInputDeviceThumbnail $
--             newDescribeInputDeviceThumbnail
--
--         , requestUpdateInputDevice $
--             newUpdateInputDevice'
--
--         , requestListInputs $
--             newListInputs
--
--         , requestCreateChannel $
--             newCreateChannel'
--
--         , requestDeleteInput $
--             newDeleteInput
--
--         , requestListInputDevices $
--             newListInputDevices
--
--         , requestUpdateInput $
--             newUpdateInput'
--
--         , requestListInputDeviceTransfers $
--             newListInputDeviceTransfers
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
--         , requestListOfferings $
--             newListOfferings
--
--         , requestUpdateMultiplex $
--             newUpdateMultiplex'
--
--         , requestDeleteMultiplex $
--             newDeleteMultiplex
--
--         , requestDeleteInputSecurityGroup $
--             newDeleteInputSecurityGroup
--
--         , requestUpdateInputSecurityGroup $
--             newUpdateInputSecurityGroup
--
--         , requestListInputSecurityGroups $
--             newListInputSecurityGroups
--
--         , requestDescribeInput $
--             newDescribeInput
--
--         , requestCreateInputSecurityGroup $
--             newCreateInputSecurityGroup
--
--         , requestStartChannel $
--             newStartChannel
--
--         , requestDescribeInputDevice $
--             newDescribeInputDevice
--
--         , requestStopChannel $
--             newStopChannel
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestBatchUpdateSchedule $
--             newBatchUpdateSchedule
--
--         , requestDescribeOffering $
--             newDescribeOffering
--
--         , requestAcceptInputDeviceTransfer $
--             newAcceptInputDeviceTransfer
--
--         , requestDeleteMultiplexProgram $
--             newDeleteMultiplexProgram
--
--         , requestUpdateMultiplexProgram $
--             newUpdateMultiplexProgram'
--
--         , requestDescribeReservation $
--             newDescribeReservation
--
--         , requestDescribeInputSecurityGroup $
--             newDescribeInputSecurityGroup
--
--         , requestListChannels $
--             newListChannels
--
--         , requestRejectInputDeviceTransfer $
--             newRejectInputDeviceTransfer
--
--         , requestCreateMultiplex $
--             newCreateMultiplex'
--
--         , requestCreatePartnerInput $
--             newCreatePartnerInput'
--
--         , requestDeleteSchedule $
--             newDeleteSchedule
--
--         , requestListMultiplexes $
--             newListMultiplexes
--
--         , requestUpdateReservation $
--             newUpdateReservation'
--
--         , requestDeleteReservation $
--             newDeleteReservation
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
--         , requestDescribeSchedule $
--             newDescribeSchedule
--
--         , requestCreateMultiplexProgram $
--             newCreateMultiplexProgram'
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
--         , requestDescribeMultiplex $
--             newDescribeMultiplex
--
--         , requestBatchDelete $
--             newBatchDelete'
--
--         , requestCreateInput $
--             newCreateInput'
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestUpdateChannel $
--             newUpdateChannel'
--
--           ]

--     , testGroup "response"
--         [ responseDescribeInputDeviceThumbnail $
--             newDescribeInputDeviceThumbnailResponse
--
--         , responseUpdateInputDevice $
--             newUpdateInputDeviceResponse
--
--         , responseListInputs $
--             newListInputsResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseDeleteInput $
--             newDeleteInputResponse
--
--         , responseListInputDevices $
--             newListInputDevicesResponse
--
--         , responseUpdateInput $
--             newUpdateInputResponse
--
--         , responseListInputDeviceTransfers $
--             newListInputDeviceTransfersResponse
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
--         , responseListOfferings $
--             newListOfferingsResponse
--
--         , responseUpdateMultiplex $
--             newUpdateMultiplexResponse
--
--         , responseDeleteMultiplex $
--             newDeleteMultiplexResponse
--
--         , responseDeleteInputSecurityGroup $
--             newDeleteInputSecurityGroupResponse
--
--         , responseUpdateInputSecurityGroup $
--             newUpdateInputSecurityGroupResponse
--
--         , responseListInputSecurityGroups $
--             newListInputSecurityGroupsResponse
--
--         , responseDescribeInput $
--             newDescribeInputResponse
--
--         , responseCreateInputSecurityGroup $
--             newCreateInputSecurityGroupResponse
--
--         , responseStartChannel $
--             newStartChannelResponse
--
--         , responseDescribeInputDevice $
--             newDescribeInputDeviceResponse
--
--         , responseStopChannel $
--             newStopChannelResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseBatchUpdateSchedule $
--             newBatchUpdateScheduleResponse
--
--         , responseDescribeOffering $
--             newDescribeOfferingResponse
--
--         , responseAcceptInputDeviceTransfer $
--             newAcceptInputDeviceTransferResponse
--
--         , responseDeleteMultiplexProgram $
--             newDeleteMultiplexProgramResponse
--
--         , responseUpdateMultiplexProgram $
--             newUpdateMultiplexProgramResponse
--
--         , responseDescribeReservation $
--             newDescribeReservationResponse
--
--         , responseDescribeInputSecurityGroup $
--             newDescribeInputSecurityGroupResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseRejectInputDeviceTransfer $
--             newRejectInputDeviceTransferResponse
--
--         , responseCreateMultiplex $
--             newCreateMultiplexResponse
--
--         , responseCreatePartnerInput $
--             newCreatePartnerInputResponse
--
--         , responseDeleteSchedule $
--             newDeleteScheduleResponse
--
--         , responseListMultiplexes $
--             newListMultiplexesResponse
--
--         , responseUpdateReservation $
--             newUpdateReservationResponse
--
--         , responseDeleteReservation $
--             newDeleteReservationResponse
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
--         , responseDescribeSchedule $
--             newDescribeScheduleResponse
--
--         , responseCreateMultiplexProgram $
--             newCreateMultiplexProgramResponse
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
--         , responseDescribeMultiplex $
--             newDescribeMultiplexResponse
--
--         , responseBatchDelete $
--             newBatchDeleteResponse
--
--         , responseCreateInput $
--             newCreateInputResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseUpdateChannel $
--             newUpdateChannelResponse
--
--           ]
--     ]

-- Requests

requestDescribeInputDeviceThumbnail :: DescribeInputDeviceThumbnail -> TestTree
requestDescribeInputDeviceThumbnail =
  req
    "DescribeInputDeviceThumbnail"
    "fixture/DescribeInputDeviceThumbnail.yaml"

requestUpdateInputDevice :: UpdateInputDevice' -> TestTree
requestUpdateInputDevice =
  req
    "UpdateInputDevice"
    "fixture/UpdateInputDevice.yaml"

requestListInputs :: ListInputs -> TestTree
requestListInputs =
  req
    "ListInputs"
    "fixture/ListInputs.yaml"

requestCreateChannel :: CreateChannel' -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDeleteInput :: DeleteInput -> TestTree
requestDeleteInput =
  req
    "DeleteInput"
    "fixture/DeleteInput.yaml"

requestListInputDevices :: ListInputDevices -> TestTree
requestListInputDevices =
  req
    "ListInputDevices"
    "fixture/ListInputDevices.yaml"

requestUpdateInput :: UpdateInput' -> TestTree
requestUpdateInput =
  req
    "UpdateInput"
    "fixture/UpdateInput.yaml"

requestListInputDeviceTransfers :: ListInputDeviceTransfers -> TestTree
requestListInputDeviceTransfers =
  req
    "ListInputDeviceTransfers"
    "fixture/ListInputDeviceTransfers.yaml"

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

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

requestUpdateMultiplex :: UpdateMultiplex' -> TestTree
requestUpdateMultiplex =
  req
    "UpdateMultiplex"
    "fixture/UpdateMultiplex.yaml"

requestDeleteMultiplex :: DeleteMultiplex -> TestTree
requestDeleteMultiplex =
  req
    "DeleteMultiplex"
    "fixture/DeleteMultiplex.yaml"

requestDeleteInputSecurityGroup :: DeleteInputSecurityGroup -> TestTree
requestDeleteInputSecurityGroup =
  req
    "DeleteInputSecurityGroup"
    "fixture/DeleteInputSecurityGroup.yaml"

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

requestDescribeInput :: DescribeInput -> TestTree
requestDescribeInput =
  req
    "DescribeInput"
    "fixture/DescribeInput.yaml"

requestCreateInputSecurityGroup :: CreateInputSecurityGroup -> TestTree
requestCreateInputSecurityGroup =
  req
    "CreateInputSecurityGroup"
    "fixture/CreateInputSecurityGroup.yaml"

requestStartChannel :: StartChannel -> TestTree
requestStartChannel =
  req
    "StartChannel"
    "fixture/StartChannel.yaml"

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

requestDescribeOffering :: DescribeOffering -> TestTree
requestDescribeOffering =
  req
    "DescribeOffering"
    "fixture/DescribeOffering.yaml"

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

requestUpdateMultiplexProgram :: UpdateMultiplexProgram' -> TestTree
requestUpdateMultiplexProgram =
  req
    "UpdateMultiplexProgram"
    "fixture/UpdateMultiplexProgram.yaml"

requestDescribeReservation :: DescribeReservation -> TestTree
requestDescribeReservation =
  req
    "DescribeReservation"
    "fixture/DescribeReservation.yaml"

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

requestRejectInputDeviceTransfer :: RejectInputDeviceTransfer -> TestTree
requestRejectInputDeviceTransfer =
  req
    "RejectInputDeviceTransfer"
    "fixture/RejectInputDeviceTransfer.yaml"

requestCreateMultiplex :: CreateMultiplex' -> TestTree
requestCreateMultiplex =
  req
    "CreateMultiplex"
    "fixture/CreateMultiplex.yaml"

requestCreatePartnerInput :: CreatePartnerInput' -> TestTree
requestCreatePartnerInput =
  req
    "CreatePartnerInput"
    "fixture/CreatePartnerInput.yaml"

requestDeleteSchedule :: DeleteSchedule -> TestTree
requestDeleteSchedule =
  req
    "DeleteSchedule"
    "fixture/DeleteSchedule.yaml"

requestListMultiplexes :: ListMultiplexes -> TestTree
requestListMultiplexes =
  req
    "ListMultiplexes"
    "fixture/ListMultiplexes.yaml"

requestUpdateReservation :: UpdateReservation' -> TestTree
requestUpdateReservation =
  req
    "UpdateReservation"
    "fixture/UpdateReservation.yaml"

requestDeleteReservation :: DeleteReservation -> TestTree
requestDeleteReservation =
  req
    "DeleteReservation"
    "fixture/DeleteReservation.yaml"

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

requestDescribeSchedule :: DescribeSchedule -> TestTree
requestDescribeSchedule =
  req
    "DescribeSchedule"
    "fixture/DescribeSchedule.yaml"

requestCreateMultiplexProgram :: CreateMultiplexProgram' -> TestTree
requestCreateMultiplexProgram =
  req
    "CreateMultiplexProgram"
    "fixture/CreateMultiplexProgram.yaml"

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

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestUpdateChannel :: UpdateChannel' -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

-- Responses

responseUpdateInputDevice :: UpdateInputDeviceResponse -> TestTree
responseUpdateInputDevice =
  res
    "UpdateInputDeviceResponse"
    "fixture/UpdateInputDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInputDevice')

responseListInputs :: ListInputsResponse -> TestTree
responseListInputs =
  res
    "ListInputsResponse"
    "fixture/ListInputsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInputs)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChannel')

responseDeleteInput :: DeleteInputResponse -> TestTree
responseDeleteInput =
  res
    "DeleteInputResponse"
    "fixture/DeleteInputResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInput)

responseListInputDevices :: ListInputDevicesResponse -> TestTree
responseListInputDevices =
  res
    "ListInputDevicesResponse"
    "fixture/ListInputDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListInputDevices)

responseUpdateInput :: UpdateInputResponse -> TestTree
responseUpdateInput =
  res
    "UpdateInputResponse"
    "fixture/UpdateInputResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInput')

responseListInputDeviceTransfers :: ListInputDeviceTransfersResponse -> TestTree
responseListInputDeviceTransfers =
  res
    "ListInputDeviceTransfersResponse"
    "fixture/ListInputDeviceTransfersResponse.proto"
    defaultService
    (Proxy :: Proxy ListInputDeviceTransfers)

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

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOfferings)

responseUpdateMultiplex :: UpdateMultiplexResponse -> TestTree
responseUpdateMultiplex =
  res
    "UpdateMultiplexResponse"
    "fixture/UpdateMultiplexResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMultiplex')

responseDeleteMultiplex :: DeleteMultiplexResponse -> TestTree
responseDeleteMultiplex =
  res
    "DeleteMultiplexResponse"
    "fixture/DeleteMultiplexResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMultiplex)

responseDeleteInputSecurityGroup :: DeleteInputSecurityGroupResponse -> TestTree
responseDeleteInputSecurityGroup =
  res
    "DeleteInputSecurityGroupResponse"
    "fixture/DeleteInputSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInputSecurityGroup)

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

responseDescribeInput :: DescribeInputResponse -> TestTree
responseDescribeInput =
  res
    "DescribeInputResponse"
    "fixture/DescribeInputResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInput)

responseCreateInputSecurityGroup :: CreateInputSecurityGroupResponse -> TestTree
responseCreateInputSecurityGroup =
  res
    "CreateInputSecurityGroupResponse"
    "fixture/CreateInputSecurityGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInputSecurityGroup)

responseStartChannel :: StartChannelResponse -> TestTree
responseStartChannel =
  res
    "StartChannelResponse"
    "fixture/StartChannelResponse.proto"
    defaultService
    (Proxy :: Proxy StartChannel)

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

responseDescribeOffering :: DescribeOfferingResponse -> TestTree
responseDescribeOffering =
  res
    "DescribeOfferingResponse"
    "fixture/DescribeOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOffering)

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

responseUpdateMultiplexProgram :: UpdateMultiplexProgramResponse -> TestTree
responseUpdateMultiplexProgram =
  res
    "UpdateMultiplexProgramResponse"
    "fixture/UpdateMultiplexProgramResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMultiplexProgram')

responseDescribeReservation :: DescribeReservationResponse -> TestTree
responseDescribeReservation =
  res
    "DescribeReservationResponse"
    "fixture/DescribeReservationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservation)

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

responseRejectInputDeviceTransfer :: RejectInputDeviceTransferResponse -> TestTree
responseRejectInputDeviceTransfer =
  res
    "RejectInputDeviceTransferResponse"
    "fixture/RejectInputDeviceTransferResponse.proto"
    defaultService
    (Proxy :: Proxy RejectInputDeviceTransfer)

responseCreateMultiplex :: CreateMultiplexResponse -> TestTree
responseCreateMultiplex =
  res
    "CreateMultiplexResponse"
    "fixture/CreateMultiplexResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMultiplex')

responseCreatePartnerInput :: CreatePartnerInputResponse -> TestTree
responseCreatePartnerInput =
  res
    "CreatePartnerInputResponse"
    "fixture/CreatePartnerInputResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePartnerInput')

responseDeleteSchedule :: DeleteScheduleResponse -> TestTree
responseDeleteSchedule =
  res
    "DeleteScheduleResponse"
    "fixture/DeleteScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSchedule)

responseListMultiplexes :: ListMultiplexesResponse -> TestTree
responseListMultiplexes =
  res
    "ListMultiplexesResponse"
    "fixture/ListMultiplexesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMultiplexes)

responseUpdateReservation :: UpdateReservationResponse -> TestTree
responseUpdateReservation =
  res
    "UpdateReservationResponse"
    "fixture/UpdateReservationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateReservation')

responseDeleteReservation :: DeleteReservationResponse -> TestTree
responseDeleteReservation =
  res
    "DeleteReservationResponse"
    "fixture/DeleteReservationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReservation)

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

responseDescribeSchedule :: DescribeScheduleResponse -> TestTree
responseDescribeSchedule =
  res
    "DescribeScheduleResponse"
    "fixture/DescribeScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSchedule)

responseCreateMultiplexProgram :: CreateMultiplexProgramResponse -> TestTree
responseCreateMultiplexProgram =
  res
    "CreateMultiplexProgramResponse"
    "fixture/CreateMultiplexProgramResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMultiplexProgram')

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

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateChannel')
