{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MediaLive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MediaLive where

import Amazonka.MediaLive
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MediaLive.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateChannelClass $
--             newUpdateChannelClass'
--
--         , requestListMultiplexes $
--             newListMultiplexes
--
--         , requestBatchStart $
--             newBatchStart'
--
--         , requestCreateMultiplex $
--             newCreateMultiplex'
--
--         , requestListInputDeviceTransfers $
--             newListInputDeviceTransfers
--
--         , requestListInputDevices $
--             newListInputDevices
--
--         , requestListInputs $
--             newListInputs
--
--         , requestDescribeInputDeviceThumbnail $
--             newDescribeInputDeviceThumbnail
--
--         , requestListChannels $
--             newListChannels
--
--         , requestDescribeInputSecurityGroup $
--             newDescribeInputSecurityGroup
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
--         , requestAcceptInputDeviceTransfer $
--             newAcceptInputDeviceTransfer
--
--         , requestDescribeReservation $
--             newDescribeReservation
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestStopMultiplex $
--             newStopMultiplex
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestCreateInputSecurityGroup $
--             newCreateInputSecurityGroup
--
--         , requestStartChannel $
--             newStartChannel
--
--         , requestCancelInputDeviceTransfer $
--             newCancelInputDeviceTransfer
--
--         , requestListInputSecurityGroups $
--             newListInputSecurityGroups
--
--         , requestDeleteReservation $
--             newDeleteReservation
--
--         , requestUpdateReservation $
--             newUpdateReservation'
--
--         , requestBatchStop $
--             newBatchStop'
--
--         , requestDeleteSchedule $
--             newDeleteSchedule
--
--         , requestCreatePartnerInput $
--             newCreatePartnerInput'
--
--         , requestCreateChannel $
--             newCreateChannel'
--
--         , requestDeleteInput $
--             newDeleteInput
--
--         , requestUpdateInput $
--             newUpdateInput'
--
--         , requestUpdateInputDevice $
--             newUpdateInputDevice'
--
--         , requestRejectInputDeviceTransfer $
--             newRejectInputDeviceTransfer
--
--         , requestClaimDevice $
--             newClaimDevice
--
--         , requestDescribeOffering $
--             newDescribeOffering
--
--         , requestTransferInputDevice $
--             newTransferInputDevice'
--
--         , requestDeleteMultiplexProgram $
--             newDeleteMultiplexProgram
--
--         , requestUpdateMultiplexProgram $
--             newUpdateMultiplexProgram'
--
--         , requestBatchDelete $
--             newBatchDelete'
--
--         , requestListMultiplexPrograms $
--             newListMultiplexPrograms
--
--         , requestDescribeMultiplex $
--             newDescribeMultiplex
--
--         , requestBatchUpdateSchedule $
--             newBatchUpdateSchedule
--
--         , requestCreateMultiplexProgram $
--             newCreateMultiplexProgram'
--
--         , requestDescribeSchedule $
--             newDescribeSchedule
--
--         , requestStartMultiplex $
--             newStartMultiplex
--
--         , requestStopChannel $
--             newStopChannel
--
--         , requestDescribeInput $
--             newDescribeInput
--
--         , requestPurchaseOffering $
--             newPurchaseOffering'
--
--         , requestDescribeInputDevice $
--             newDescribeInputDevice
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestUpdateInputSecurityGroup $
--             newUpdateInputSecurityGroup
--
--         , requestDeleteInputSecurityGroup $
--             newDeleteInputSecurityGroup
--
--         , requestListReservations $
--             newListReservations
--
--         , requestDeleteMultiplex $
--             newDeleteMultiplex
--
--         , requestUpdateMultiplex $
--             newUpdateMultiplex'
--
--         , requestDescribeMultiplexProgram $
--             newDescribeMultiplexProgram
--
--         , requestListOfferings $
--             newListOfferings
--
--           ]

--     , testGroup "response"
--         [ responseUpdateChannelClass $
--             newUpdateChannelClassResponse
--
--         , responseListMultiplexes $
--             newListMultiplexesResponse
--
--         , responseBatchStart $
--             newBatchStartResponse
--
--         , responseCreateMultiplex $
--             newCreateMultiplexResponse
--
--         , responseListInputDeviceTransfers $
--             newListInputDeviceTransfersResponse
--
--         , responseListInputDevices $
--             newListInputDevicesResponse
--
--         , responseListInputs $
--             newListInputsResponse
--
--         , responseDescribeInputDeviceThumbnail $
--             newDescribeInputDeviceThumbnailResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseDescribeInputSecurityGroup $
--             newDescribeInputSecurityGroupResponse
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
--         , responseAcceptInputDeviceTransfer $
--             newAcceptInputDeviceTransferResponse
--
--         , responseDescribeReservation $
--             newDescribeReservationResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseStopMultiplex $
--             newStopMultiplexResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseCreateInputSecurityGroup $
--             newCreateInputSecurityGroupResponse
--
--         , responseStartChannel $
--             newStartChannelResponse
--
--         , responseCancelInputDeviceTransfer $
--             newCancelInputDeviceTransferResponse
--
--         , responseListInputSecurityGroups $
--             newListInputSecurityGroupsResponse
--
--         , responseDeleteReservation $
--             newDeleteReservationResponse
--
--         , responseUpdateReservation $
--             newUpdateReservationResponse
--
--         , responseBatchStop $
--             newBatchStopResponse
--
--         , responseDeleteSchedule $
--             newDeleteScheduleResponse
--
--         , responseCreatePartnerInput $
--             newCreatePartnerInputResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseDeleteInput $
--             newDeleteInputResponse
--
--         , responseUpdateInput $
--             newUpdateInputResponse
--
--         , responseUpdateInputDevice $
--             newUpdateInputDeviceResponse
--
--         , responseRejectInputDeviceTransfer $
--             newRejectInputDeviceTransferResponse
--
--         , responseClaimDevice $
--             newClaimDeviceResponse
--
--         , responseDescribeOffering $
--             newDescribeOfferingResponse
--
--         , responseTransferInputDevice $
--             newTransferInputDeviceResponse
--
--         , responseDeleteMultiplexProgram $
--             newDeleteMultiplexProgramResponse
--
--         , responseUpdateMultiplexProgram $
--             newUpdateMultiplexProgramResponse
--
--         , responseBatchDelete $
--             newBatchDeleteResponse
--
--         , responseListMultiplexPrograms $
--             newListMultiplexProgramsResponse
--
--         , responseDescribeMultiplex $
--             newDescribeMultiplexResponse
--
--         , responseBatchUpdateSchedule $
--             newBatchUpdateScheduleResponse
--
--         , responseCreateMultiplexProgram $
--             newCreateMultiplexProgramResponse
--
--         , responseDescribeSchedule $
--             newDescribeScheduleResponse
--
--         , responseStartMultiplex $
--             newStartMultiplexResponse
--
--         , responseStopChannel $
--             newStopChannelResponse
--
--         , responseDescribeInput $
--             newDescribeInputResponse
--
--         , responsePurchaseOffering $
--             newPurchaseOfferingResponse
--
--         , responseDescribeInputDevice $
--             newDescribeInputDeviceResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseUpdateInputSecurityGroup $
--             newUpdateInputSecurityGroupResponse
--
--         , responseDeleteInputSecurityGroup $
--             newDeleteInputSecurityGroupResponse
--
--         , responseListReservations $
--             newListReservationsResponse
--
--         , responseDeleteMultiplex $
--             newDeleteMultiplexResponse
--
--         , responseUpdateMultiplex $
--             newUpdateMultiplexResponse
--
--         , responseDescribeMultiplexProgram $
--             newDescribeMultiplexProgramResponse
--
--         , responseListOfferings $
--             newListOfferingsResponse
--
--           ]
--     ]

-- Requests

requestUpdateChannelClass :: UpdateChannelClass' -> TestTree
requestUpdateChannelClass =
  req
    "UpdateChannelClass"
    "fixture/UpdateChannelClass.yaml"

requestListMultiplexes :: ListMultiplexes -> TestTree
requestListMultiplexes =
  req
    "ListMultiplexes"
    "fixture/ListMultiplexes.yaml"

requestBatchStart :: BatchStart' -> TestTree
requestBatchStart =
  req
    "BatchStart"
    "fixture/BatchStart.yaml"

requestCreateMultiplex :: CreateMultiplex' -> TestTree
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

requestUpdateReservation :: UpdateReservation' -> TestTree
requestUpdateReservation =
  req
    "UpdateReservation"
    "fixture/UpdateReservation.yaml"

requestBatchStop :: BatchStop' -> TestTree
requestBatchStop =
  req
    "BatchStop"
    "fixture/BatchStop.yaml"

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

requestUpdateInput :: UpdateInput' -> TestTree
requestUpdateInput =
  req
    "UpdateInput"
    "fixture/UpdateInput.yaml"

requestUpdateInputDevice :: UpdateInputDevice' -> TestTree
requestUpdateInputDevice =
  req
    "UpdateInputDevice"
    "fixture/UpdateInputDevice.yaml"

requestRejectInputDeviceTransfer :: RejectInputDeviceTransfer -> TestTree
requestRejectInputDeviceTransfer =
  req
    "RejectInputDeviceTransfer"
    "fixture/RejectInputDeviceTransfer.yaml"

requestClaimDevice :: ClaimDevice -> TestTree
requestClaimDevice =
  req
    "ClaimDevice"
    "fixture/ClaimDevice.yaml"

requestDescribeOffering :: DescribeOffering -> TestTree
requestDescribeOffering =
  req
    "DescribeOffering"
    "fixture/DescribeOffering.yaml"

requestTransferInputDevice :: TransferInputDevice' -> TestTree
requestTransferInputDevice =
  req
    "TransferInputDevice"
    "fixture/TransferInputDevice.yaml"

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

requestBatchDelete :: BatchDelete' -> TestTree
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

requestPurchaseOffering :: PurchaseOffering' -> TestTree
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

requestUpdateMultiplex :: UpdateMultiplex' -> TestTree
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelClass')

responseListMultiplexes :: ListMultiplexesResponse -> TestTree
responseListMultiplexes =
  res
    "ListMultiplexesResponse"
    "fixture/ListMultiplexesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMultiplexes)

responseBatchStart :: BatchStartResponse -> TestTree
responseBatchStart =
  res
    "BatchStartResponse"
    "fixture/BatchStartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchStart')

responseCreateMultiplex :: CreateMultiplexResponse -> TestTree
responseCreateMultiplex =
  res
    "CreateMultiplexResponse"
    "fixture/CreateMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMultiplex')

responseListInputDeviceTransfers :: ListInputDeviceTransfersResponse -> TestTree
responseListInputDeviceTransfers =
  res
    "ListInputDeviceTransfersResponse"
    "fixture/ListInputDeviceTransfersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputDeviceTransfers)

responseListInputDevices :: ListInputDevicesResponse -> TestTree
responseListInputDevices =
  res
    "ListInputDevicesResponse"
    "fixture/ListInputDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputDevices)

responseListInputs :: ListInputsResponse -> TestTree
responseListInputs =
  res
    "ListInputsResponse"
    "fixture/ListInputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputs)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannels)

responseDescribeInputSecurityGroup :: DescribeInputSecurityGroupResponse -> TestTree
responseDescribeInputSecurityGroup =
  res
    "DescribeInputSecurityGroupResponse"
    "fixture/DescribeInputSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInputSecurityGroup)

responseCreateInput :: CreateInputResponse -> TestTree
responseCreateInput =
  res
    "CreateInputResponse"
    "fixture/CreateInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInput')

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannel')

responseAcceptInputDeviceTransfer :: AcceptInputDeviceTransferResponse -> TestTree
responseAcceptInputDeviceTransfer =
  res
    "AcceptInputDeviceTransferResponse"
    "fixture/AcceptInputDeviceTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInputDeviceTransfer)

responseDescribeReservation :: DescribeReservationResponse -> TestTree
responseDescribeReservation =
  res
    "DescribeReservationResponse"
    "fixture/DescribeReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservation)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTags)

responseStopMultiplex :: StopMultiplexResponse -> TestTree
responseStopMultiplex =
  res
    "StopMultiplexResponse"
    "fixture/StopMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMultiplex)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseCreateInputSecurityGroup :: CreateInputSecurityGroupResponse -> TestTree
responseCreateInputSecurityGroup =
  res
    "CreateInputSecurityGroupResponse"
    "fixture/CreateInputSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInputSecurityGroup)

responseStartChannel :: StartChannelResponse -> TestTree
responseStartChannel =
  res
    "StartChannelResponse"
    "fixture/StartChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartChannel)

responseCancelInputDeviceTransfer :: CancelInputDeviceTransferResponse -> TestTree
responseCancelInputDeviceTransfer =
  res
    "CancelInputDeviceTransferResponse"
    "fixture/CancelInputDeviceTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelInputDeviceTransfer)

responseListInputSecurityGroups :: ListInputSecurityGroupsResponse -> TestTree
responseListInputSecurityGroups =
  res
    "ListInputSecurityGroupsResponse"
    "fixture/ListInputSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputSecurityGroups)

responseDeleteReservation :: DeleteReservationResponse -> TestTree
responseDeleteReservation =
  res
    "DeleteReservationResponse"
    "fixture/DeleteReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReservation)

responseUpdateReservation :: UpdateReservationResponse -> TestTree
responseUpdateReservation =
  res
    "UpdateReservationResponse"
    "fixture/UpdateReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReservation')

responseBatchStop :: BatchStopResponse -> TestTree
responseBatchStop =
  res
    "BatchStopResponse"
    "fixture/BatchStopResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchStop')

responseDeleteSchedule :: DeleteScheduleResponse -> TestTree
responseDeleteSchedule =
  res
    "DeleteScheduleResponse"
    "fixture/DeleteScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchedule)

responseCreatePartnerInput :: CreatePartnerInputResponse -> TestTree
responseCreatePartnerInput =
  res
    "CreatePartnerInputResponse"
    "fixture/CreatePartnerInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePartnerInput')

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel')

responseDeleteInput :: DeleteInputResponse -> TestTree
responseDeleteInput =
  res
    "DeleteInputResponse"
    "fixture/DeleteInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInput)

responseUpdateInput :: UpdateInputResponse -> TestTree
responseUpdateInput =
  res
    "UpdateInputResponse"
    "fixture/UpdateInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInput')

responseUpdateInputDevice :: UpdateInputDeviceResponse -> TestTree
responseUpdateInputDevice =
  res
    "UpdateInputDeviceResponse"
    "fixture/UpdateInputDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInputDevice')

responseRejectInputDeviceTransfer :: RejectInputDeviceTransferResponse -> TestTree
responseRejectInputDeviceTransfer =
  res
    "RejectInputDeviceTransferResponse"
    "fixture/RejectInputDeviceTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectInputDeviceTransfer)

responseClaimDevice :: ClaimDeviceResponse -> TestTree
responseClaimDevice =
  res
    "ClaimDeviceResponse"
    "fixture/ClaimDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClaimDevice)

responseDescribeOffering :: DescribeOfferingResponse -> TestTree
responseDescribeOffering =
  res
    "DescribeOfferingResponse"
    "fixture/DescribeOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOffering)

responseTransferInputDevice :: TransferInputDeviceResponse -> TestTree
responseTransferInputDevice =
  res
    "TransferInputDeviceResponse"
    "fixture/TransferInputDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransferInputDevice')

responseDeleteMultiplexProgram :: DeleteMultiplexProgramResponse -> TestTree
responseDeleteMultiplexProgram =
  res
    "DeleteMultiplexProgramResponse"
    "fixture/DeleteMultiplexProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMultiplexProgram)

responseUpdateMultiplexProgram :: UpdateMultiplexProgramResponse -> TestTree
responseUpdateMultiplexProgram =
  res
    "UpdateMultiplexProgramResponse"
    "fixture/UpdateMultiplexProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMultiplexProgram')

responseBatchDelete :: BatchDeleteResponse -> TestTree
responseBatchDelete =
  res
    "BatchDeleteResponse"
    "fixture/BatchDeleteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDelete')

responseListMultiplexPrograms :: ListMultiplexProgramsResponse -> TestTree
responseListMultiplexPrograms =
  res
    "ListMultiplexProgramsResponse"
    "fixture/ListMultiplexProgramsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMultiplexPrograms)

responseDescribeMultiplex :: DescribeMultiplexResponse -> TestTree
responseDescribeMultiplex =
  res
    "DescribeMultiplexResponse"
    "fixture/DescribeMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMultiplex)

responseBatchUpdateSchedule :: BatchUpdateScheduleResponse -> TestTree
responseBatchUpdateSchedule =
  res
    "BatchUpdateScheduleResponse"
    "fixture/BatchUpdateScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateSchedule)

responseCreateMultiplexProgram :: CreateMultiplexProgramResponse -> TestTree
responseCreateMultiplexProgram =
  res
    "CreateMultiplexProgramResponse"
    "fixture/CreateMultiplexProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMultiplexProgram')

responseDescribeSchedule :: DescribeScheduleResponse -> TestTree
responseDescribeSchedule =
  res
    "DescribeScheduleResponse"
    "fixture/DescribeScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSchedule)

responseStartMultiplex :: StartMultiplexResponse -> TestTree
responseStartMultiplex =
  res
    "StartMultiplexResponse"
    "fixture/StartMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMultiplex)

responseStopChannel :: StopChannelResponse -> TestTree
responseStopChannel =
  res
    "StopChannelResponse"
    "fixture/StopChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopChannel)

responseDescribeInput :: DescribeInputResponse -> TestTree
responseDescribeInput =
  res
    "DescribeInputResponse"
    "fixture/DescribeInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInput)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseOffering')

responseDescribeInputDevice :: DescribeInputDeviceResponse -> TestTree
responseDescribeInputDevice =
  res
    "DescribeInputDeviceResponse"
    "fixture/DescribeInputDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInputDevice)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannel)

responseUpdateInputSecurityGroup :: UpdateInputSecurityGroupResponse -> TestTree
responseUpdateInputSecurityGroup =
  res
    "UpdateInputSecurityGroupResponse"
    "fixture/UpdateInputSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInputSecurityGroup)

responseDeleteInputSecurityGroup :: DeleteInputSecurityGroupResponse -> TestTree
responseDeleteInputSecurityGroup =
  res
    "DeleteInputSecurityGroupResponse"
    "fixture/DeleteInputSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInputSecurityGroup)

responseListReservations :: ListReservationsResponse -> TestTree
responseListReservations =
  res
    "ListReservationsResponse"
    "fixture/ListReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReservations)

responseDeleteMultiplex :: DeleteMultiplexResponse -> TestTree
responseDeleteMultiplex =
  res
    "DeleteMultiplexResponse"
    "fixture/DeleteMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMultiplex)

responseUpdateMultiplex :: UpdateMultiplexResponse -> TestTree
responseUpdateMultiplex =
  res
    "UpdateMultiplexResponse"
    "fixture/UpdateMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMultiplex')

responseDescribeMultiplexProgram :: DescribeMultiplexProgramResponse -> TestTree
responseDescribeMultiplexProgram =
  res
    "DescribeMultiplexProgramResponse"
    "fixture/DescribeMultiplexProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMultiplexProgram)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferings)
