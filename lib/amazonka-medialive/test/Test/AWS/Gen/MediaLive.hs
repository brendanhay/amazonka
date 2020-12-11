{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaLive
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestUpdateChannelClass $
--             mkUpdateChannelClass
--
--         , requestListMultiplexes $
--             mkListMultiplexes
--
--         , requestBatchStart $
--             mkBatchStart
--
--         , requestCreateMultiplex $
--             mkCreateMultiplex
--
--         , requestListInputDeviceTransfers $
--             mkListInputDeviceTransfers
--
--         , requestListInputDevices $
--             mkListInputDevices
--
--         , requestListInputs $
--             mkListInputs
--
--         , requestDescribeInputDeviceThumbnail $
--             mkDescribeInputDeviceThumbnail
--
--         , requestListChannels $
--             mkListChannels
--
--         , requestDescribeInputSecurityGroup $
--             mkDescribeInputSecurityGroup
--
--         , requestCreateInput $
--             mkCreateInput
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDeleteChannel $
--             mkDeleteChannel
--
--         , requestUpdateChannel $
--             mkUpdateChannel
--
--         , requestAcceptInputDeviceTransfer $
--             mkAcceptInputDeviceTransfer
--
--         , requestDescribeReservation $
--             mkDescribeReservation
--
--         , requestCreateTags $
--             mkCreateTags
--
--         , requestStopMultiplex $
--             mkStopMultiplex
--
--         , requestDeleteTags $
--             mkDeleteTags
--
--         , requestCreateInputSecurityGroup $
--             mkCreateInputSecurityGroup
--
--         , requestStartChannel $
--             mkStartChannel
--
--         , requestCancelInputDeviceTransfer $
--             mkCancelInputDeviceTransfer
--
--         , requestListInputSecurityGroups $
--             mkListInputSecurityGroups
--
--         , requestDeleteReservation $
--             mkDeleteReservation
--
--         , requestUpdateReservation $
--             mkUpdateReservation
--
--         , requestBatchStop $
--             mkBatchStop
--
--         , requestDeleteSchedule $
--             mkDeleteSchedule
--
--         , requestCreateChannel $
--             mkCreateChannel
--
--         , requestDeleteInput $
--             mkDeleteInput
--
--         , requestUpdateInput $
--             mkUpdateInput
--
--         , requestUpdateInputDevice $
--             mkUpdateInputDevice
--
--         , requestRejectInputDeviceTransfer $
--             mkRejectInputDeviceTransfer
--
--         , requestDescribeOffering $
--             mkDescribeOffering
--
--         , requestTransferInputDevice $
--             mkTransferInputDevice
--
--         , requestDeleteMultiplexProgram $
--             mkDeleteMultiplexProgram
--
--         , requestUpdateMultiplexProgram $
--             mkUpdateMultiplexProgram
--
--         , requestBatchDelete $
--             mkBatchDelete
--
--         , requestListMultiplexPrograms $
--             mkListMultiplexPrograms
--
--         , requestDescribeMultiplex $
--             mkDescribeMultiplex
--
--         , requestBatchUpdateSchedule $
--             mkBatchUpdateSchedule
--
--         , requestCreateMultiplexProgram $
--             mkCreateMultiplexProgram
--
--         , requestDescribeSchedule $
--             mkDescribeSchedule
--
--         , requestStartMultiplex $
--             mkStartMultiplex
--
--         , requestStopChannel $
--             mkStopChannel
--
--         , requestDescribeInput $
--             mkDescribeInput
--
--         , requestPurchaseOffering $
--             mkPurchaseOffering
--
--         , requestDescribeInputDevice $
--             mkDescribeInputDevice
--
--         , requestDescribeChannel $
--             mkDescribeChannel
--
--         , requestUpdateInputSecurityGroup $
--             mkUpdateInputSecurityGroup
--
--         , requestDeleteInputSecurityGroup $
--             mkDeleteInputSecurityGroup
--
--         , requestListReservations $
--             mkListReservations
--
--         , requestDeleteMultiplex $
--             mkDeleteMultiplex
--
--         , requestUpdateMultiplex $
--             mkUpdateMultiplex
--
--         , requestDescribeMultiplexProgram $
--             mkDescribeMultiplexProgram
--
--         , requestListOfferings $
--             mkListOfferings
--
--           ]

--     , testGroup "response"
--         [ responseUpdateChannelClass $
--             mkUpdateChannelClassResponse
--
--         , responseListMultiplexes $
--             mkListMultiplexesResponse
--
--         , responseBatchStart $
--             mkBatchStartResponse
--
--         , responseCreateMultiplex $
--             mkCreateMultiplexResponse
--
--         , responseListInputDeviceTransfers $
--             mkListInputDeviceTransfersResponse
--
--         , responseListInputDevices $
--             mkListInputDevicesResponse
--
--         , responseListInputs $
--             mkListInputsResponse
--
--         , responseDescribeInputDeviceThumbnail $
--             mkDescribeInputDeviceThumbnailResponse
--
--         , responseListChannels $
--             mkListChannelsResponse
--
--         , responseDescribeInputSecurityGroup $
--             mkDescribeInputSecurityGroupResponse
--
--         , responseCreateInput $
--             mkCreateInputResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDeleteChannel $
--             mkDeleteChannelResponse
--
--         , responseUpdateChannel $
--             mkUpdateChannelResponse
--
--         , responseAcceptInputDeviceTransfer $
--             mkAcceptInputDeviceTransferResponse
--
--         , responseDescribeReservation $
--             mkDescribeReservationResponse
--
--         , responseCreateTags $
--             mkCreateTagsResponse
--
--         , responseStopMultiplex $
--             mkStopMultiplexResponse
--
--         , responseDeleteTags $
--             mkDeleteTagsResponse
--
--         , responseCreateInputSecurityGroup $
--             mkCreateInputSecurityGroupResponse
--
--         , responseStartChannel $
--             mkStartChannelResponse
--
--         , responseCancelInputDeviceTransfer $
--             mkCancelInputDeviceTransferResponse
--
--         , responseListInputSecurityGroups $
--             mkListInputSecurityGroupsResponse
--
--         , responseDeleteReservation $
--             mkDeleteReservationResponse
--
--         , responseUpdateReservation $
--             mkUpdateReservationResponse
--
--         , responseBatchStop $
--             mkBatchStopResponse
--
--         , responseDeleteSchedule $
--             mkDeleteScheduleResponse
--
--         , responseCreateChannel $
--             mkCreateChannelResponse
--
--         , responseDeleteInput $
--             mkDeleteInputResponse
--
--         , responseUpdateInput $
--             mkUpdateInputResponse
--
--         , responseUpdateInputDevice $
--             mkUpdateInputDeviceResponse
--
--         , responseRejectInputDeviceTransfer $
--             mkRejectInputDeviceTransferResponse
--
--         , responseDescribeOffering $
--             mkDescribeOfferingResponse
--
--         , responseTransferInputDevice $
--             mkTransferInputDeviceResponse
--
--         , responseDeleteMultiplexProgram $
--             mkDeleteMultiplexProgramResponse
--
--         , responseUpdateMultiplexProgram $
--             mkUpdateMultiplexProgramResponse
--
--         , responseBatchDelete $
--             mkBatchDeleteResponse
--
--         , responseListMultiplexPrograms $
--             mkListMultiplexProgramsResponse
--
--         , responseDescribeMultiplex $
--             mkDescribeMultiplexResponse
--
--         , responseBatchUpdateSchedule $
--             mkBatchUpdateScheduleResponse
--
--         , responseCreateMultiplexProgram $
--             mkCreateMultiplexProgramResponse
--
--         , responseDescribeSchedule $
--             mkDescribeScheduleResponse
--
--         , responseStartMultiplex $
--             mkStartMultiplexResponse
--
--         , responseStopChannel $
--             mkStopChannelResponse
--
--         , responseDescribeInput $
--             mkDescribeInputResponse
--
--         , responsePurchaseOffering $
--             mkPurchaseOfferingResponse
--
--         , responseDescribeInputDevice $
--             mkDescribeInputDeviceResponse
--
--         , responseDescribeChannel $
--             mkDescribeChannelResponse
--
--         , responseUpdateInputSecurityGroup $
--             mkUpdateInputSecurityGroupResponse
--
--         , responseDeleteInputSecurityGroup $
--             mkDeleteInputSecurityGroupResponse
--
--         , responseListReservations $
--             mkListReservationsResponse
--
--         , responseDeleteMultiplex $
--             mkDeleteMultiplexResponse
--
--         , responseUpdateMultiplex $
--             mkUpdateMultiplexResponse
--
--         , responseDescribeMultiplexProgram $
--             mkDescribeMultiplexProgramResponse
--
--         , responseListOfferings $
--             mkListOfferingsResponse
--
--           ]
--     ]

-- Requests

requestUpdateChannelClass :: UpdateChannelClass -> TestTree
requestUpdateChannelClass =
  req
    "UpdateChannelClass"
    "fixture/UpdateChannelClass.yaml"

requestListMultiplexes :: ListMultiplexes -> TestTree
requestListMultiplexes =
  req
    "ListMultiplexes"
    "fixture/ListMultiplexes.yaml"

requestBatchStart :: BatchStart -> TestTree
requestBatchStart =
  req
    "BatchStart"
    "fixture/BatchStart.yaml"

requestCreateMultiplex :: CreateMultiplex -> TestTree
requestCreateMultiplex =
  req
    "CreateMultiplex"
    "fixture/CreateMultiplex.yaml"

requestListInputDeviceTransfers :: ListInputDeviceTransfers -> TestTree
requestListInputDeviceTransfers =
  req
    "ListInputDeviceTransfers"
    "fixture/ListInputDeviceTransfers.yaml"

requestListInputDevices :: ListInputDevices -> TestTree
requestListInputDevices =
  req
    "ListInputDevices"
    "fixture/ListInputDevices.yaml"

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

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestDescribeInputSecurityGroup :: DescribeInputSecurityGroup -> TestTree
requestDescribeInputSecurityGroup =
  req
    "DescribeInputSecurityGroup"
    "fixture/DescribeInputSecurityGroup.yaml"

requestCreateInput :: CreateInput -> TestTree
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

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestAcceptInputDeviceTransfer :: AcceptInputDeviceTransfer -> TestTree
requestAcceptInputDeviceTransfer =
  req
    "AcceptInputDeviceTransfer"
    "fixture/AcceptInputDeviceTransfer.yaml"

requestDescribeReservation :: DescribeReservation -> TestTree
requestDescribeReservation =
  req
    "DescribeReservation"
    "fixture/DescribeReservation.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestStopMultiplex :: StopMultiplex -> TestTree
requestStopMultiplex =
  req
    "StopMultiplex"
    "fixture/StopMultiplex.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

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

requestCancelInputDeviceTransfer :: CancelInputDeviceTransfer -> TestTree
requestCancelInputDeviceTransfer =
  req
    "CancelInputDeviceTransfer"
    "fixture/CancelInputDeviceTransfer.yaml"

requestListInputSecurityGroups :: ListInputSecurityGroups -> TestTree
requestListInputSecurityGroups =
  req
    "ListInputSecurityGroups"
    "fixture/ListInputSecurityGroups.yaml"

requestDeleteReservation :: DeleteReservation -> TestTree
requestDeleteReservation =
  req
    "DeleteReservation"
    "fixture/DeleteReservation.yaml"

requestUpdateReservation :: UpdateReservation -> TestTree
requestUpdateReservation =
  req
    "UpdateReservation"
    "fixture/UpdateReservation.yaml"

requestBatchStop :: BatchStop -> TestTree
requestBatchStop =
  req
    "BatchStop"
    "fixture/BatchStop.yaml"

requestDeleteSchedule :: DeleteSchedule -> TestTree
requestDeleteSchedule =
  req
    "DeleteSchedule"
    "fixture/DeleteSchedule.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDeleteInput :: DeleteInput -> TestTree
requestDeleteInput =
  req
    "DeleteInput"
    "fixture/DeleteInput.yaml"

requestUpdateInput :: UpdateInput -> TestTree
requestUpdateInput =
  req
    "UpdateInput"
    "fixture/UpdateInput.yaml"

requestUpdateInputDevice :: UpdateInputDevice -> TestTree
requestUpdateInputDevice =
  req
    "UpdateInputDevice"
    "fixture/UpdateInputDevice.yaml"

requestRejectInputDeviceTransfer :: RejectInputDeviceTransfer -> TestTree
requestRejectInputDeviceTransfer =
  req
    "RejectInputDeviceTransfer"
    "fixture/RejectInputDeviceTransfer.yaml"

requestDescribeOffering :: DescribeOffering -> TestTree
requestDescribeOffering =
  req
    "DescribeOffering"
    "fixture/DescribeOffering.yaml"

requestTransferInputDevice :: TransferInputDevice -> TestTree
requestTransferInputDevice =
  req
    "TransferInputDevice"
    "fixture/TransferInputDevice.yaml"

requestDeleteMultiplexProgram :: DeleteMultiplexProgram -> TestTree
requestDeleteMultiplexProgram =
  req
    "DeleteMultiplexProgram"
    "fixture/DeleteMultiplexProgram.yaml"

requestUpdateMultiplexProgram :: UpdateMultiplexProgram -> TestTree
requestUpdateMultiplexProgram =
  req
    "UpdateMultiplexProgram"
    "fixture/UpdateMultiplexProgram.yaml"

requestBatchDelete :: BatchDelete -> TestTree
requestBatchDelete =
  req
    "BatchDelete"
    "fixture/BatchDelete.yaml"

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

requestBatchUpdateSchedule :: BatchUpdateSchedule -> TestTree
requestBatchUpdateSchedule =
  req
    "BatchUpdateSchedule"
    "fixture/BatchUpdateSchedule.yaml"

requestCreateMultiplexProgram :: CreateMultiplexProgram -> TestTree
requestCreateMultiplexProgram =
  req
    "CreateMultiplexProgram"
    "fixture/CreateMultiplexProgram.yaml"

requestDescribeSchedule :: DescribeSchedule -> TestTree
requestDescribeSchedule =
  req
    "DescribeSchedule"
    "fixture/DescribeSchedule.yaml"

requestStartMultiplex :: StartMultiplex -> TestTree
requestStartMultiplex =
  req
    "StartMultiplex"
    "fixture/StartMultiplex.yaml"

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

requestPurchaseOffering :: PurchaseOffering -> TestTree
requestPurchaseOffering =
  req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestDescribeInputDevice :: DescribeInputDevice -> TestTree
requestDescribeInputDevice =
  req
    "DescribeInputDevice"
    "fixture/DescribeInputDevice.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestUpdateInputSecurityGroup :: UpdateInputSecurityGroup -> TestTree
requestUpdateInputSecurityGroup =
  req
    "UpdateInputSecurityGroup"
    "fixture/UpdateInputSecurityGroup.yaml"

requestDeleteInputSecurityGroup :: DeleteInputSecurityGroup -> TestTree
requestDeleteInputSecurityGroup =
  req
    "DeleteInputSecurityGroup"
    "fixture/DeleteInputSecurityGroup.yaml"

requestListReservations :: ListReservations -> TestTree
requestListReservations =
  req
    "ListReservations"
    "fixture/ListReservations.yaml"

requestDeleteMultiplex :: DeleteMultiplex -> TestTree
requestDeleteMultiplex =
  req
    "DeleteMultiplex"
    "fixture/DeleteMultiplex.yaml"

requestUpdateMultiplex :: UpdateMultiplex -> TestTree
requestUpdateMultiplex =
  req
    "UpdateMultiplex"
    "fixture/UpdateMultiplex.yaml"

requestDescribeMultiplexProgram :: DescribeMultiplexProgram -> TestTree
requestDescribeMultiplexProgram =
  req
    "DescribeMultiplexProgram"
    "fixture/DescribeMultiplexProgram.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

-- Responses

responseUpdateChannelClass :: UpdateChannelClassResponse -> TestTree
responseUpdateChannelClass =
  res
    "UpdateChannelClassResponse"
    "fixture/UpdateChannelClassResponse.proto"
    mediaLiveService
    (Proxy :: Proxy UpdateChannelClass)

responseListMultiplexes :: ListMultiplexesResponse -> TestTree
responseListMultiplexes =
  res
    "ListMultiplexesResponse"
    "fixture/ListMultiplexesResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListMultiplexes)

responseBatchStart :: BatchStartResponse -> TestTree
responseBatchStart =
  res
    "BatchStartResponse"
    "fixture/BatchStartResponse.proto"
    mediaLiveService
    (Proxy :: Proxy BatchStart)

responseCreateMultiplex :: CreateMultiplexResponse -> TestTree
responseCreateMultiplex =
  res
    "CreateMultiplexResponse"
    "fixture/CreateMultiplexResponse.proto"
    mediaLiveService
    (Proxy :: Proxy CreateMultiplex)

responseListInputDeviceTransfers :: ListInputDeviceTransfersResponse -> TestTree
responseListInputDeviceTransfers =
  res
    "ListInputDeviceTransfersResponse"
    "fixture/ListInputDeviceTransfersResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListInputDeviceTransfers)

responseListInputDevices :: ListInputDevicesResponse -> TestTree
responseListInputDevices =
  res
    "ListInputDevicesResponse"
    "fixture/ListInputDevicesResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListInputDevices)

responseListInputs :: ListInputsResponse -> TestTree
responseListInputs =
  res
    "ListInputsResponse"
    "fixture/ListInputsResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListInputs)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListChannels)

responseDescribeInputSecurityGroup :: DescribeInputSecurityGroupResponse -> TestTree
responseDescribeInputSecurityGroup =
  res
    "DescribeInputSecurityGroupResponse"
    "fixture/DescribeInputSecurityGroupResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DescribeInputSecurityGroup)

responseCreateInput :: CreateInputResponse -> TestTree
responseCreateInput =
  res
    "CreateInputResponse"
    "fixture/CreateInputResponse.proto"
    mediaLiveService
    (Proxy :: Proxy CreateInput)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListTagsForResource)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    mediaLiveService
    (Proxy :: Proxy UpdateChannel)

responseAcceptInputDeviceTransfer :: AcceptInputDeviceTransferResponse -> TestTree
responseAcceptInputDeviceTransfer =
  res
    "AcceptInputDeviceTransferResponse"
    "fixture/AcceptInputDeviceTransferResponse.proto"
    mediaLiveService
    (Proxy :: Proxy AcceptInputDeviceTransfer)

responseDescribeReservation :: DescribeReservationResponse -> TestTree
responseDescribeReservation =
  res
    "DescribeReservationResponse"
    "fixture/DescribeReservationResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DescribeReservation)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    mediaLiveService
    (Proxy :: Proxy CreateTags)

responseStopMultiplex :: StopMultiplexResponse -> TestTree
responseStopMultiplex =
  res
    "StopMultiplexResponse"
    "fixture/StopMultiplexResponse.proto"
    mediaLiveService
    (Proxy :: Proxy StopMultiplex)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DeleteTags)

responseCreateInputSecurityGroup :: CreateInputSecurityGroupResponse -> TestTree
responseCreateInputSecurityGroup =
  res
    "CreateInputSecurityGroupResponse"
    "fixture/CreateInputSecurityGroupResponse.proto"
    mediaLiveService
    (Proxy :: Proxy CreateInputSecurityGroup)

responseStartChannel :: StartChannelResponse -> TestTree
responseStartChannel =
  res
    "StartChannelResponse"
    "fixture/StartChannelResponse.proto"
    mediaLiveService
    (Proxy :: Proxy StartChannel)

responseCancelInputDeviceTransfer :: CancelInputDeviceTransferResponse -> TestTree
responseCancelInputDeviceTransfer =
  res
    "CancelInputDeviceTransferResponse"
    "fixture/CancelInputDeviceTransferResponse.proto"
    mediaLiveService
    (Proxy :: Proxy CancelInputDeviceTransfer)

responseListInputSecurityGroups :: ListInputSecurityGroupsResponse -> TestTree
responseListInputSecurityGroups =
  res
    "ListInputSecurityGroupsResponse"
    "fixture/ListInputSecurityGroupsResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListInputSecurityGroups)

responseDeleteReservation :: DeleteReservationResponse -> TestTree
responseDeleteReservation =
  res
    "DeleteReservationResponse"
    "fixture/DeleteReservationResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DeleteReservation)

responseUpdateReservation :: UpdateReservationResponse -> TestTree
responseUpdateReservation =
  res
    "UpdateReservationResponse"
    "fixture/UpdateReservationResponse.proto"
    mediaLiveService
    (Proxy :: Proxy UpdateReservation)

responseBatchStop :: BatchStopResponse -> TestTree
responseBatchStop =
  res
    "BatchStopResponse"
    "fixture/BatchStopResponse.proto"
    mediaLiveService
    (Proxy :: Proxy BatchStop)

responseDeleteSchedule :: DeleteScheduleResponse -> TestTree
responseDeleteSchedule =
  res
    "DeleteScheduleResponse"
    "fixture/DeleteScheduleResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DeleteSchedule)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    mediaLiveService
    (Proxy :: Proxy CreateChannel)

responseDeleteInput :: DeleteInputResponse -> TestTree
responseDeleteInput =
  res
    "DeleteInputResponse"
    "fixture/DeleteInputResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DeleteInput)

responseUpdateInput :: UpdateInputResponse -> TestTree
responseUpdateInput =
  res
    "UpdateInputResponse"
    "fixture/UpdateInputResponse.proto"
    mediaLiveService
    (Proxy :: Proxy UpdateInput)

responseUpdateInputDevice :: UpdateInputDeviceResponse -> TestTree
responseUpdateInputDevice =
  res
    "UpdateInputDeviceResponse"
    "fixture/UpdateInputDeviceResponse.proto"
    mediaLiveService
    (Proxy :: Proxy UpdateInputDevice)

responseRejectInputDeviceTransfer :: RejectInputDeviceTransferResponse -> TestTree
responseRejectInputDeviceTransfer =
  res
    "RejectInputDeviceTransferResponse"
    "fixture/RejectInputDeviceTransferResponse.proto"
    mediaLiveService
    (Proxy :: Proxy RejectInputDeviceTransfer)

responseDescribeOffering :: DescribeOfferingResponse -> TestTree
responseDescribeOffering =
  res
    "DescribeOfferingResponse"
    "fixture/DescribeOfferingResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DescribeOffering)

responseTransferInputDevice :: TransferInputDeviceResponse -> TestTree
responseTransferInputDevice =
  res
    "TransferInputDeviceResponse"
    "fixture/TransferInputDeviceResponse.proto"
    mediaLiveService
    (Proxy :: Proxy TransferInputDevice)

responseDeleteMultiplexProgram :: DeleteMultiplexProgramResponse -> TestTree
responseDeleteMultiplexProgram =
  res
    "DeleteMultiplexProgramResponse"
    "fixture/DeleteMultiplexProgramResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DeleteMultiplexProgram)

responseUpdateMultiplexProgram :: UpdateMultiplexProgramResponse -> TestTree
responseUpdateMultiplexProgram =
  res
    "UpdateMultiplexProgramResponse"
    "fixture/UpdateMultiplexProgramResponse.proto"
    mediaLiveService
    (Proxy :: Proxy UpdateMultiplexProgram)

responseBatchDelete :: BatchDeleteResponse -> TestTree
responseBatchDelete =
  res
    "BatchDeleteResponse"
    "fixture/BatchDeleteResponse.proto"
    mediaLiveService
    (Proxy :: Proxy BatchDelete)

responseListMultiplexPrograms :: ListMultiplexProgramsResponse -> TestTree
responseListMultiplexPrograms =
  res
    "ListMultiplexProgramsResponse"
    "fixture/ListMultiplexProgramsResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListMultiplexPrograms)

responseDescribeMultiplex :: DescribeMultiplexResponse -> TestTree
responseDescribeMultiplex =
  res
    "DescribeMultiplexResponse"
    "fixture/DescribeMultiplexResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DescribeMultiplex)

responseBatchUpdateSchedule :: BatchUpdateScheduleResponse -> TestTree
responseBatchUpdateSchedule =
  res
    "BatchUpdateScheduleResponse"
    "fixture/BatchUpdateScheduleResponse.proto"
    mediaLiveService
    (Proxy :: Proxy BatchUpdateSchedule)

responseCreateMultiplexProgram :: CreateMultiplexProgramResponse -> TestTree
responseCreateMultiplexProgram =
  res
    "CreateMultiplexProgramResponse"
    "fixture/CreateMultiplexProgramResponse.proto"
    mediaLiveService
    (Proxy :: Proxy CreateMultiplexProgram)

responseDescribeSchedule :: DescribeScheduleResponse -> TestTree
responseDescribeSchedule =
  res
    "DescribeScheduleResponse"
    "fixture/DescribeScheduleResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DescribeSchedule)

responseStartMultiplex :: StartMultiplexResponse -> TestTree
responseStartMultiplex =
  res
    "StartMultiplexResponse"
    "fixture/StartMultiplexResponse.proto"
    mediaLiveService
    (Proxy :: Proxy StartMultiplex)

responseStopChannel :: StopChannelResponse -> TestTree
responseStopChannel =
  res
    "StopChannelResponse"
    "fixture/StopChannelResponse.proto"
    mediaLiveService
    (Proxy :: Proxy StopChannel)

responseDescribeInput :: DescribeInputResponse -> TestTree
responseDescribeInput =
  res
    "DescribeInputResponse"
    "fixture/DescribeInputResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DescribeInput)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    mediaLiveService
    (Proxy :: Proxy PurchaseOffering)

responseDescribeInputDevice :: DescribeInputDeviceResponse -> TestTree
responseDescribeInputDevice =
  res
    "DescribeInputDeviceResponse"
    "fixture/DescribeInputDeviceResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DescribeInputDevice)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DescribeChannel)

responseUpdateInputSecurityGroup :: UpdateInputSecurityGroupResponse -> TestTree
responseUpdateInputSecurityGroup =
  res
    "UpdateInputSecurityGroupResponse"
    "fixture/UpdateInputSecurityGroupResponse.proto"
    mediaLiveService
    (Proxy :: Proxy UpdateInputSecurityGroup)

responseDeleteInputSecurityGroup :: DeleteInputSecurityGroupResponse -> TestTree
responseDeleteInputSecurityGroup =
  res
    "DeleteInputSecurityGroupResponse"
    "fixture/DeleteInputSecurityGroupResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DeleteInputSecurityGroup)

responseListReservations :: ListReservationsResponse -> TestTree
responseListReservations =
  res
    "ListReservationsResponse"
    "fixture/ListReservationsResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListReservations)

responseDeleteMultiplex :: DeleteMultiplexResponse -> TestTree
responseDeleteMultiplex =
  res
    "DeleteMultiplexResponse"
    "fixture/DeleteMultiplexResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DeleteMultiplex)

responseUpdateMultiplex :: UpdateMultiplexResponse -> TestTree
responseUpdateMultiplex =
  res
    "UpdateMultiplexResponse"
    "fixture/UpdateMultiplexResponse.proto"
    mediaLiveService
    (Proxy :: Proxy UpdateMultiplex)

responseDescribeMultiplexProgram :: DescribeMultiplexProgramResponse -> TestTree
responseDescribeMultiplexProgram =
  res
    "DescribeMultiplexProgramResponse"
    "fixture/DescribeMultiplexProgramResponse.proto"
    mediaLiveService
    (Proxy :: Proxy DescribeMultiplexProgram)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    mediaLiveService
    (Proxy :: Proxy ListOfferings)
