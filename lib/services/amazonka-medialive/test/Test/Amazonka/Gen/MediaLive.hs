{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MediaLive
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestAcceptInputDeviceTransfer $
--             newAcceptInputDeviceTransfer
--
--         , requestBatchDelete $
--             newBatchDelete'
--
--         , requestBatchStart $
--             newBatchStart'
--
--         , requestBatchStop $
--             newBatchStop'
--
--         , requestBatchUpdateSchedule $
--             newBatchUpdateSchedule
--
--         , requestCancelInputDeviceTransfer $
--             newCancelInputDeviceTransfer
--
--         , requestClaimDevice $
--             newClaimDevice
--
--         , requestCreateChannel $
--             newCreateChannel'
--
--         , requestCreateInput $
--             newCreateInput'
--
--         , requestCreateInputSecurityGroup $
--             newCreateInputSecurityGroup
--
--         , requestCreateMultiplex $
--             newCreateMultiplex'
--
--         , requestCreateMultiplexProgram $
--             newCreateMultiplexProgram'
--
--         , requestCreatePartnerInput $
--             newCreatePartnerInput'
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestDeleteInput $
--             newDeleteInput
--
--         , requestDeleteInputSecurityGroup $
--             newDeleteInputSecurityGroup
--
--         , requestDeleteMultiplex $
--             newDeleteMultiplex
--
--         , requestDeleteMultiplexProgram $
--             newDeleteMultiplexProgram
--
--         , requestDeleteReservation $
--             newDeleteReservation
--
--         , requestDeleteSchedule $
--             newDeleteSchedule
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestDescribeInput $
--             newDescribeInput
--
--         , requestDescribeInputDevice $
--             newDescribeInputDevice
--
--         , requestDescribeInputDeviceThumbnail $
--             newDescribeInputDeviceThumbnail
--
--         , requestDescribeInputSecurityGroup $
--             newDescribeInputSecurityGroup
--
--         , requestDescribeMultiplex $
--             newDescribeMultiplex
--
--         , requestDescribeMultiplexProgram $
--             newDescribeMultiplexProgram
--
--         , requestDescribeOffering $
--             newDescribeOffering
--
--         , requestDescribeReservation $
--             newDescribeReservation
--
--         , requestDescribeSchedule $
--             newDescribeSchedule
--
--         , requestListChannels $
--             newListChannels
--
--         , requestListInputDeviceTransfers $
--             newListInputDeviceTransfers
--
--         , requestListInputDevices $
--             newListInputDevices
--
--         , requestListInputSecurityGroups $
--             newListInputSecurityGroups
--
--         , requestListInputs $
--             newListInputs
--
--         , requestListMultiplexPrograms $
--             newListMultiplexPrograms
--
--         , requestListMultiplexes $
--             newListMultiplexes
--
--         , requestListOfferings $
--             newListOfferings
--
--         , requestListReservations $
--             newListReservations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPurchaseOffering $
--             newPurchaseOffering'
--
--         , requestRebootInputDevice $
--             newRebootInputDevice'
--
--         , requestRejectInputDeviceTransfer $
--             newRejectInputDeviceTransfer
--
--         , requestStartChannel $
--             newStartChannel
--
--         , requestStartInputDeviceMaintenanceWindow $
--             newStartInputDeviceMaintenanceWindow
--
--         , requestStartMultiplex $
--             newStartMultiplex
--
--         , requestStopChannel $
--             newStopChannel
--
--         , requestStopMultiplex $
--             newStopMultiplex
--
--         , requestTransferInputDevice $
--             newTransferInputDevice'
--
--         , requestUpdateChannel $
--             newUpdateChannel'
--
--         , requestUpdateChannelClass $
--             newUpdateChannelClass'
--
--         , requestUpdateInput $
--             newUpdateInput'
--
--         , requestUpdateInputDevice $
--             newUpdateInputDevice'
--
--         , requestUpdateInputSecurityGroup $
--             newUpdateInputSecurityGroup
--
--         , requestUpdateMultiplex $
--             newUpdateMultiplex'
--
--         , requestUpdateMultiplexProgram $
--             newUpdateMultiplexProgram'
--
--         , requestUpdateReservation $
--             newUpdateReservation'
--
--           ]

--     , testGroup "response"
--         [ responseAcceptInputDeviceTransfer $
--             newAcceptInputDeviceTransferResponse
--
--         , responseBatchDelete $
--             newBatchDeleteResponse
--
--         , responseBatchStart $
--             newBatchStartResponse
--
--         , responseBatchStop $
--             newBatchStopResponse
--
--         , responseBatchUpdateSchedule $
--             newBatchUpdateScheduleResponse
--
--         , responseCancelInputDeviceTransfer $
--             newCancelInputDeviceTransferResponse
--
--         , responseClaimDevice $
--             newClaimDeviceResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseCreateInput $
--             newCreateInputResponse
--
--         , responseCreateInputSecurityGroup $
--             newCreateInputSecurityGroupResponse
--
--         , responseCreateMultiplex $
--             newCreateMultiplexResponse
--
--         , responseCreateMultiplexProgram $
--             newCreateMultiplexProgramResponse
--
--         , responseCreatePartnerInput $
--             newCreatePartnerInputResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseDeleteInput $
--             newDeleteInputResponse
--
--         , responseDeleteInputSecurityGroup $
--             newDeleteInputSecurityGroupResponse
--
--         , responseDeleteMultiplex $
--             newDeleteMultiplexResponse
--
--         , responseDeleteMultiplexProgram $
--             newDeleteMultiplexProgramResponse
--
--         , responseDeleteReservation $
--             newDeleteReservationResponse
--
--         , responseDeleteSchedule $
--             newDeleteScheduleResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseDescribeInput $
--             newDescribeInputResponse
--
--         , responseDescribeInputDevice $
--             newDescribeInputDeviceResponse
--
--         , responseDescribeInputDeviceThumbnail $
--             newDescribeInputDeviceThumbnailResponse
--
--         , responseDescribeInputSecurityGroup $
--             newDescribeInputSecurityGroupResponse
--
--         , responseDescribeMultiplex $
--             newDescribeMultiplexResponse
--
--         , responseDescribeMultiplexProgram $
--             newDescribeMultiplexProgramResponse
--
--         , responseDescribeOffering $
--             newDescribeOfferingResponse
--
--         , responseDescribeReservation $
--             newDescribeReservationResponse
--
--         , responseDescribeSchedule $
--             newDescribeScheduleResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseListInputDeviceTransfers $
--             newListInputDeviceTransfersResponse
--
--         , responseListInputDevices $
--             newListInputDevicesResponse
--
--         , responseListInputSecurityGroups $
--             newListInputSecurityGroupsResponse
--
--         , responseListInputs $
--             newListInputsResponse
--
--         , responseListMultiplexPrograms $
--             newListMultiplexProgramsResponse
--
--         , responseListMultiplexes $
--             newListMultiplexesResponse
--
--         , responseListOfferings $
--             newListOfferingsResponse
--
--         , responseListReservations $
--             newListReservationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePurchaseOffering $
--             newPurchaseOfferingResponse
--
--         , responseRebootInputDevice $
--             newRebootInputDeviceResponse
--
--         , responseRejectInputDeviceTransfer $
--             newRejectInputDeviceTransferResponse
--
--         , responseStartChannel $
--             newStartChannelResponse
--
--         , responseStartInputDeviceMaintenanceWindow $
--             newStartInputDeviceMaintenanceWindowResponse
--
--         , responseStartMultiplex $
--             newStartMultiplexResponse
--
--         , responseStopChannel $
--             newStopChannelResponse
--
--         , responseStopMultiplex $
--             newStopMultiplexResponse
--
--         , responseTransferInputDevice $
--             newTransferInputDeviceResponse
--
--         , responseUpdateChannel $
--             newUpdateChannelResponse
--
--         , responseUpdateChannelClass $
--             newUpdateChannelClassResponse
--
--         , responseUpdateInput $
--             newUpdateInputResponse
--
--         , responseUpdateInputDevice $
--             newUpdateInputDeviceResponse
--
--         , responseUpdateInputSecurityGroup $
--             newUpdateInputSecurityGroupResponse
--
--         , responseUpdateMultiplex $
--             newUpdateMultiplexResponse
--
--         , responseUpdateMultiplexProgram $
--             newUpdateMultiplexProgramResponse
--
--         , responseUpdateReservation $
--             newUpdateReservationResponse
--
--           ]
--     ]

-- Requests

requestAcceptInputDeviceTransfer :: AcceptInputDeviceTransfer -> TestTree
requestAcceptInputDeviceTransfer =
  req
    "AcceptInputDeviceTransfer"
    "fixture/AcceptInputDeviceTransfer.yaml"

requestBatchDelete :: BatchDelete' -> TestTree
requestBatchDelete =
  req
    "BatchDelete"
    "fixture/BatchDelete.yaml"

requestBatchStart :: BatchStart' -> TestTree
requestBatchStart =
  req
    "BatchStart"
    "fixture/BatchStart.yaml"

requestBatchStop :: BatchStop' -> TestTree
requestBatchStop =
  req
    "BatchStop"
    "fixture/BatchStop.yaml"

requestBatchUpdateSchedule :: BatchUpdateSchedule -> TestTree
requestBatchUpdateSchedule =
  req
    "BatchUpdateSchedule"
    "fixture/BatchUpdateSchedule.yaml"

requestCancelInputDeviceTransfer :: CancelInputDeviceTransfer -> TestTree
requestCancelInputDeviceTransfer =
  req
    "CancelInputDeviceTransfer"
    "fixture/CancelInputDeviceTransfer.yaml"

requestClaimDevice :: ClaimDevice -> TestTree
requestClaimDevice =
  req
    "ClaimDevice"
    "fixture/ClaimDevice.yaml"

requestCreateChannel :: CreateChannel' -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestCreateInput :: CreateInput' -> TestTree
requestCreateInput =
  req
    "CreateInput"
    "fixture/CreateInput.yaml"

requestCreateInputSecurityGroup :: CreateInputSecurityGroup -> TestTree
requestCreateInputSecurityGroup =
  req
    "CreateInputSecurityGroup"
    "fixture/CreateInputSecurityGroup.yaml"

requestCreateMultiplex :: CreateMultiplex' -> TestTree
requestCreateMultiplex =
  req
    "CreateMultiplex"
    "fixture/CreateMultiplex.yaml"

requestCreateMultiplexProgram :: CreateMultiplexProgram' -> TestTree
requestCreateMultiplexProgram =
  req
    "CreateMultiplexProgram"
    "fixture/CreateMultiplexProgram.yaml"

requestCreatePartnerInput :: CreatePartnerInput' -> TestTree
requestCreatePartnerInput =
  req
    "CreatePartnerInput"
    "fixture/CreatePartnerInput.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestDeleteInput :: DeleteInput -> TestTree
requestDeleteInput =
  req
    "DeleteInput"
    "fixture/DeleteInput.yaml"

requestDeleteInputSecurityGroup :: DeleteInputSecurityGroup -> TestTree
requestDeleteInputSecurityGroup =
  req
    "DeleteInputSecurityGroup"
    "fixture/DeleteInputSecurityGroup.yaml"

requestDeleteMultiplex :: DeleteMultiplex -> TestTree
requestDeleteMultiplex =
  req
    "DeleteMultiplex"
    "fixture/DeleteMultiplex.yaml"

requestDeleteMultiplexProgram :: DeleteMultiplexProgram -> TestTree
requestDeleteMultiplexProgram =
  req
    "DeleteMultiplexProgram"
    "fixture/DeleteMultiplexProgram.yaml"

requestDeleteReservation :: DeleteReservation -> TestTree
requestDeleteReservation =
  req
    "DeleteReservation"
    "fixture/DeleteReservation.yaml"

requestDeleteSchedule :: DeleteSchedule -> TestTree
requestDeleteSchedule =
  req
    "DeleteSchedule"
    "fixture/DeleteSchedule.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestDescribeInput :: DescribeInput -> TestTree
requestDescribeInput =
  req
    "DescribeInput"
    "fixture/DescribeInput.yaml"

requestDescribeInputDevice :: DescribeInputDevice -> TestTree
requestDescribeInputDevice =
  req
    "DescribeInputDevice"
    "fixture/DescribeInputDevice.yaml"

requestDescribeInputDeviceThumbnail :: DescribeInputDeviceThumbnail -> TestTree
requestDescribeInputDeviceThumbnail =
  req
    "DescribeInputDeviceThumbnail"
    "fixture/DescribeInputDeviceThumbnail.yaml"

requestDescribeInputSecurityGroup :: DescribeInputSecurityGroup -> TestTree
requestDescribeInputSecurityGroup =
  req
    "DescribeInputSecurityGroup"
    "fixture/DescribeInputSecurityGroup.yaml"

requestDescribeMultiplex :: DescribeMultiplex -> TestTree
requestDescribeMultiplex =
  req
    "DescribeMultiplex"
    "fixture/DescribeMultiplex.yaml"

requestDescribeMultiplexProgram :: DescribeMultiplexProgram -> TestTree
requestDescribeMultiplexProgram =
  req
    "DescribeMultiplexProgram"
    "fixture/DescribeMultiplexProgram.yaml"

requestDescribeOffering :: DescribeOffering -> TestTree
requestDescribeOffering =
  req
    "DescribeOffering"
    "fixture/DescribeOffering.yaml"

requestDescribeReservation :: DescribeReservation -> TestTree
requestDescribeReservation =
  req
    "DescribeReservation"
    "fixture/DescribeReservation.yaml"

requestDescribeSchedule :: DescribeSchedule -> TestTree
requestDescribeSchedule =
  req
    "DescribeSchedule"
    "fixture/DescribeSchedule.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

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

requestListInputSecurityGroups :: ListInputSecurityGroups -> TestTree
requestListInputSecurityGroups =
  req
    "ListInputSecurityGroups"
    "fixture/ListInputSecurityGroups.yaml"

requestListInputs :: ListInputs -> TestTree
requestListInputs =
  req
    "ListInputs"
    "fixture/ListInputs.yaml"

requestListMultiplexPrograms :: ListMultiplexPrograms -> TestTree
requestListMultiplexPrograms =
  req
    "ListMultiplexPrograms"
    "fixture/ListMultiplexPrograms.yaml"

requestListMultiplexes :: ListMultiplexes -> TestTree
requestListMultiplexes =
  req
    "ListMultiplexes"
    "fixture/ListMultiplexes.yaml"

requestListOfferings :: ListOfferings -> TestTree
requestListOfferings =
  req
    "ListOfferings"
    "fixture/ListOfferings.yaml"

requestListReservations :: ListReservations -> TestTree
requestListReservations =
  req
    "ListReservations"
    "fixture/ListReservations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPurchaseOffering :: PurchaseOffering' -> TestTree
requestPurchaseOffering =
  req
    "PurchaseOffering"
    "fixture/PurchaseOffering.yaml"

requestRebootInputDevice :: RebootInputDevice' -> TestTree
requestRebootInputDevice =
  req
    "RebootInputDevice"
    "fixture/RebootInputDevice.yaml"

requestRejectInputDeviceTransfer :: RejectInputDeviceTransfer -> TestTree
requestRejectInputDeviceTransfer =
  req
    "RejectInputDeviceTransfer"
    "fixture/RejectInputDeviceTransfer.yaml"

requestStartChannel :: StartChannel -> TestTree
requestStartChannel =
  req
    "StartChannel"
    "fixture/StartChannel.yaml"

requestStartInputDeviceMaintenanceWindow :: StartInputDeviceMaintenanceWindow -> TestTree
requestStartInputDeviceMaintenanceWindow =
  req
    "StartInputDeviceMaintenanceWindow"
    "fixture/StartInputDeviceMaintenanceWindow.yaml"

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

requestStopMultiplex :: StopMultiplex -> TestTree
requestStopMultiplex =
  req
    "StopMultiplex"
    "fixture/StopMultiplex.yaml"

requestTransferInputDevice :: TransferInputDevice' -> TestTree
requestTransferInputDevice =
  req
    "TransferInputDevice"
    "fixture/TransferInputDevice.yaml"

requestUpdateChannel :: UpdateChannel' -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestUpdateChannelClass :: UpdateChannelClass' -> TestTree
requestUpdateChannelClass =
  req
    "UpdateChannelClass"
    "fixture/UpdateChannelClass.yaml"

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

requestUpdateInputSecurityGroup :: UpdateInputSecurityGroup -> TestTree
requestUpdateInputSecurityGroup =
  req
    "UpdateInputSecurityGroup"
    "fixture/UpdateInputSecurityGroup.yaml"

requestUpdateMultiplex :: UpdateMultiplex' -> TestTree
requestUpdateMultiplex =
  req
    "UpdateMultiplex"
    "fixture/UpdateMultiplex.yaml"

requestUpdateMultiplexProgram :: UpdateMultiplexProgram' -> TestTree
requestUpdateMultiplexProgram =
  req
    "UpdateMultiplexProgram"
    "fixture/UpdateMultiplexProgram.yaml"

requestUpdateReservation :: UpdateReservation' -> TestTree
requestUpdateReservation =
  req
    "UpdateReservation"
    "fixture/UpdateReservation.yaml"

-- Responses

responseAcceptInputDeviceTransfer :: AcceptInputDeviceTransferResponse -> TestTree
responseAcceptInputDeviceTransfer =
  res
    "AcceptInputDeviceTransferResponse"
    "fixture/AcceptInputDeviceTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInputDeviceTransfer)

responseBatchDelete :: BatchDeleteResponse -> TestTree
responseBatchDelete =
  res
    "BatchDeleteResponse"
    "fixture/BatchDeleteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDelete')

responseBatchStart :: BatchStartResponse -> TestTree
responseBatchStart =
  res
    "BatchStartResponse"
    "fixture/BatchStartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchStart')

responseBatchStop :: BatchStopResponse -> TestTree
responseBatchStop =
  res
    "BatchStopResponse"
    "fixture/BatchStopResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchStop')

responseBatchUpdateSchedule :: BatchUpdateScheduleResponse -> TestTree
responseBatchUpdateSchedule =
  res
    "BatchUpdateScheduleResponse"
    "fixture/BatchUpdateScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateSchedule)

responseCancelInputDeviceTransfer :: CancelInputDeviceTransferResponse -> TestTree
responseCancelInputDeviceTransfer =
  res
    "CancelInputDeviceTransferResponse"
    "fixture/CancelInputDeviceTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelInputDeviceTransfer)

responseClaimDevice :: ClaimDeviceResponse -> TestTree
responseClaimDevice =
  res
    "ClaimDeviceResponse"
    "fixture/ClaimDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ClaimDevice)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel')

responseCreateInput :: CreateInputResponse -> TestTree
responseCreateInput =
  res
    "CreateInputResponse"
    "fixture/CreateInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInput')

responseCreateInputSecurityGroup :: CreateInputSecurityGroupResponse -> TestTree
responseCreateInputSecurityGroup =
  res
    "CreateInputSecurityGroupResponse"
    "fixture/CreateInputSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInputSecurityGroup)

responseCreateMultiplex :: CreateMultiplexResponse -> TestTree
responseCreateMultiplex =
  res
    "CreateMultiplexResponse"
    "fixture/CreateMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMultiplex')

responseCreateMultiplexProgram :: CreateMultiplexProgramResponse -> TestTree
responseCreateMultiplexProgram =
  res
    "CreateMultiplexProgramResponse"
    "fixture/CreateMultiplexProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMultiplexProgram')

responseCreatePartnerInput :: CreatePartnerInputResponse -> TestTree
responseCreatePartnerInput =
  res
    "CreatePartnerInputResponse"
    "fixture/CreatePartnerInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePartnerInput')

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTags)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannel)

responseDeleteInput :: DeleteInputResponse -> TestTree
responseDeleteInput =
  res
    "DeleteInputResponse"
    "fixture/DeleteInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInput)

responseDeleteInputSecurityGroup :: DeleteInputSecurityGroupResponse -> TestTree
responseDeleteInputSecurityGroup =
  res
    "DeleteInputSecurityGroupResponse"
    "fixture/DeleteInputSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInputSecurityGroup)

responseDeleteMultiplex :: DeleteMultiplexResponse -> TestTree
responseDeleteMultiplex =
  res
    "DeleteMultiplexResponse"
    "fixture/DeleteMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMultiplex)

responseDeleteMultiplexProgram :: DeleteMultiplexProgramResponse -> TestTree
responseDeleteMultiplexProgram =
  res
    "DeleteMultiplexProgramResponse"
    "fixture/DeleteMultiplexProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMultiplexProgram)

responseDeleteReservation :: DeleteReservationResponse -> TestTree
responseDeleteReservation =
  res
    "DeleteReservationResponse"
    "fixture/DeleteReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReservation)

responseDeleteSchedule :: DeleteScheduleResponse -> TestTree
responseDeleteSchedule =
  res
    "DeleteScheduleResponse"
    "fixture/DeleteScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchedule)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannel)

responseDescribeInput :: DescribeInputResponse -> TestTree
responseDescribeInput =
  res
    "DescribeInputResponse"
    "fixture/DescribeInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInput)

responseDescribeInputDevice :: DescribeInputDeviceResponse -> TestTree
responseDescribeInputDevice =
  res
    "DescribeInputDeviceResponse"
    "fixture/DescribeInputDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInputDevice)

responseDescribeInputSecurityGroup :: DescribeInputSecurityGroupResponse -> TestTree
responseDescribeInputSecurityGroup =
  res
    "DescribeInputSecurityGroupResponse"
    "fixture/DescribeInputSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInputSecurityGroup)

responseDescribeMultiplex :: DescribeMultiplexResponse -> TestTree
responseDescribeMultiplex =
  res
    "DescribeMultiplexResponse"
    "fixture/DescribeMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMultiplex)

responseDescribeMultiplexProgram :: DescribeMultiplexProgramResponse -> TestTree
responseDescribeMultiplexProgram =
  res
    "DescribeMultiplexProgramResponse"
    "fixture/DescribeMultiplexProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMultiplexProgram)

responseDescribeOffering :: DescribeOfferingResponse -> TestTree
responseDescribeOffering =
  res
    "DescribeOfferingResponse"
    "fixture/DescribeOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOffering)

responseDescribeReservation :: DescribeReservationResponse -> TestTree
responseDescribeReservation =
  res
    "DescribeReservationResponse"
    "fixture/DescribeReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservation)

responseDescribeSchedule :: DescribeScheduleResponse -> TestTree
responseDescribeSchedule =
  res
    "DescribeScheduleResponse"
    "fixture/DescribeScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSchedule)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannels)

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

responseListInputSecurityGroups :: ListInputSecurityGroupsResponse -> TestTree
responseListInputSecurityGroups =
  res
    "ListInputSecurityGroupsResponse"
    "fixture/ListInputSecurityGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputSecurityGroups)

responseListInputs :: ListInputsResponse -> TestTree
responseListInputs =
  res
    "ListInputsResponse"
    "fixture/ListInputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInputs)

responseListMultiplexPrograms :: ListMultiplexProgramsResponse -> TestTree
responseListMultiplexPrograms =
  res
    "ListMultiplexProgramsResponse"
    "fixture/ListMultiplexProgramsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMultiplexPrograms)

responseListMultiplexes :: ListMultiplexesResponse -> TestTree
responseListMultiplexes =
  res
    "ListMultiplexesResponse"
    "fixture/ListMultiplexesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMultiplexes)

responseListOfferings :: ListOfferingsResponse -> TestTree
responseListOfferings =
  res
    "ListOfferingsResponse"
    "fixture/ListOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOfferings)

responseListReservations :: ListReservationsResponse -> TestTree
responseListReservations =
  res
    "ListReservationsResponse"
    "fixture/ListReservationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReservations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePurchaseOffering :: PurchaseOfferingResponse -> TestTree
responsePurchaseOffering =
  res
    "PurchaseOfferingResponse"
    "fixture/PurchaseOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseOffering')

responseRebootInputDevice :: RebootInputDeviceResponse -> TestTree
responseRebootInputDevice =
  res
    "RebootInputDeviceResponse"
    "fixture/RebootInputDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootInputDevice')

responseRejectInputDeviceTransfer :: RejectInputDeviceTransferResponse -> TestTree
responseRejectInputDeviceTransfer =
  res
    "RejectInputDeviceTransferResponse"
    "fixture/RejectInputDeviceTransferResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectInputDeviceTransfer)

responseStartChannel :: StartChannelResponse -> TestTree
responseStartChannel =
  res
    "StartChannelResponse"
    "fixture/StartChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartChannel)

responseStartInputDeviceMaintenanceWindow :: StartInputDeviceMaintenanceWindowResponse -> TestTree
responseStartInputDeviceMaintenanceWindow =
  res
    "StartInputDeviceMaintenanceWindowResponse"
    "fixture/StartInputDeviceMaintenanceWindowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInputDeviceMaintenanceWindow)

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

responseStopMultiplex :: StopMultiplexResponse -> TestTree
responseStopMultiplex =
  res
    "StopMultiplexResponse"
    "fixture/StopMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMultiplex)

responseTransferInputDevice :: TransferInputDeviceResponse -> TestTree
responseTransferInputDevice =
  res
    "TransferInputDeviceResponse"
    "fixture/TransferInputDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TransferInputDevice')

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannel')

responseUpdateChannelClass :: UpdateChannelClassResponse -> TestTree
responseUpdateChannelClass =
  res
    "UpdateChannelClassResponse"
    "fixture/UpdateChannelClassResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannelClass')

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

responseUpdateInputSecurityGroup :: UpdateInputSecurityGroupResponse -> TestTree
responseUpdateInputSecurityGroup =
  res
    "UpdateInputSecurityGroupResponse"
    "fixture/UpdateInputSecurityGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInputSecurityGroup)

responseUpdateMultiplex :: UpdateMultiplexResponse -> TestTree
responseUpdateMultiplex =
  res
    "UpdateMultiplexResponse"
    "fixture/UpdateMultiplexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMultiplex')

responseUpdateMultiplexProgram :: UpdateMultiplexProgramResponse -> TestTree
responseUpdateMultiplexProgram =
  res
    "UpdateMultiplexProgramResponse"
    "fixture/UpdateMultiplexProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMultiplexProgram')

responseUpdateReservation :: UpdateReservationResponse -> TestTree
responseUpdateReservation =
  res
    "UpdateReservationResponse"
    "fixture/UpdateReservationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReservation')
