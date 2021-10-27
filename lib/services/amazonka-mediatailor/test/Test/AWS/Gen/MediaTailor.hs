{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaTailor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MediaTailor where

import Data.Proxy
import Network.AWS.MediaTailor
import Test.AWS.Fixture
import Test.AWS.MediaTailor.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateSourceLocation $
--             newCreateSourceLocation
--
--         , requestListPrefetchSchedules $
--             newListPrefetchSchedules
--
--         , requestDeletePrefetchSchedule $
--             newDeletePrefetchSchedule
--
--         , requestListAlerts $
--             newListAlerts
--
--         , requestListChannels $
--             newListChannels
--
--         , requestCreatePrefetchSchedule $
--             newCreatePrefetchSchedule
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestUpdateChannel $
--             newUpdateChannel
--
--         , requestGetChannelPolicy $
--             newGetChannelPolicy
--
--         , requestDescribeVodSource $
--             newDescribeVodSource
--
--         , requestDescribeSourceLocation $
--             newDescribeSourceLocation
--
--         , requestGetPrefetchSchedule $
--             newGetPrefetchSchedule
--
--         , requestCreateProgram $
--             newCreateProgram
--
--         , requestStartChannel $
--             newStartChannel
--
--         , requestListPlaybackConfigurations $
--             newListPlaybackConfigurations
--
--         , requestDeletePlaybackConfiguration $
--             newDeletePlaybackConfiguration
--
--         , requestPutPlaybackConfiguration $
--             newPutPlaybackConfiguration
--
--         , requestListSourceLocations $
--             newListSourceLocations
--
--         , requestUpdateSourceLocation $
--             newUpdateSourceLocation
--
--         , requestDeleteSourceLocation $
--             newDeleteSourceLocation
--
--         , requestGetPlaybackConfiguration $
--             newGetPlaybackConfiguration
--
--         , requestDeleteVodSource $
--             newDeleteVodSource
--
--         , requestUpdateVodSource $
--             newUpdateVodSource
--
--         , requestCreateVodSource $
--             newCreateVodSource
--
--         , requestCreateChannel $
--             newCreateChannel
--
--         , requestDeleteChannelPolicy $
--             newDeleteChannelPolicy
--
--         , requestPutChannelPolicy $
--             newPutChannelPolicy
--
--         , requestDeleteProgram $
--             newDeleteProgram
--
--         , requestGetChannelSchedule $
--             newGetChannelSchedule
--
--         , requestTagResource $
--             newTagResource
--
--         , requestConfigureLogsForPlaybackConfiguration $
--             newConfigureLogsForPlaybackConfiguration
--
--         , requestStopChannel $
--             newStopChannel
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestListVodSources $
--             newListVodSources
--
--         , requestDescribeProgram $
--             newDescribeProgram
--
--           ]

--     , testGroup "response"
--         [ responseCreateSourceLocation $
--             newCreateSourceLocationResponse
--
--         , responseListPrefetchSchedules $
--             newListPrefetchSchedulesResponse
--
--         , responseDeletePrefetchSchedule $
--             newDeletePrefetchScheduleResponse
--
--         , responseListAlerts $
--             newListAlertsResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseCreatePrefetchSchedule $
--             newCreatePrefetchScheduleResponse
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
--         , responseGetChannelPolicy $
--             newGetChannelPolicyResponse
--
--         , responseDescribeVodSource $
--             newDescribeVodSourceResponse
--
--         , responseDescribeSourceLocation $
--             newDescribeSourceLocationResponse
--
--         , responseGetPrefetchSchedule $
--             newGetPrefetchScheduleResponse
--
--         , responseCreateProgram $
--             newCreateProgramResponse
--
--         , responseStartChannel $
--             newStartChannelResponse
--
--         , responseListPlaybackConfigurations $
--             newListPlaybackConfigurationsResponse
--
--         , responseDeletePlaybackConfiguration $
--             newDeletePlaybackConfigurationResponse
--
--         , responsePutPlaybackConfiguration $
--             newPutPlaybackConfigurationResponse
--
--         , responseListSourceLocations $
--             newListSourceLocationsResponse
--
--         , responseUpdateSourceLocation $
--             newUpdateSourceLocationResponse
--
--         , responseDeleteSourceLocation $
--             newDeleteSourceLocationResponse
--
--         , responseGetPlaybackConfiguration $
--             newGetPlaybackConfigurationResponse
--
--         , responseDeleteVodSource $
--             newDeleteVodSourceResponse
--
--         , responseUpdateVodSource $
--             newUpdateVodSourceResponse
--
--         , responseCreateVodSource $
--             newCreateVodSourceResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseDeleteChannelPolicy $
--             newDeleteChannelPolicyResponse
--
--         , responsePutChannelPolicy $
--             newPutChannelPolicyResponse
--
--         , responseDeleteProgram $
--             newDeleteProgramResponse
--
--         , responseGetChannelSchedule $
--             newGetChannelScheduleResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseConfigureLogsForPlaybackConfiguration $
--             newConfigureLogsForPlaybackConfigurationResponse
--
--         , responseStopChannel $
--             newStopChannelResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseListVodSources $
--             newListVodSourcesResponse
--
--         , responseDescribeProgram $
--             newDescribeProgramResponse
--
--           ]
--     ]

-- Requests

requestCreateSourceLocation :: CreateSourceLocation -> TestTree
requestCreateSourceLocation =
  req
    "CreateSourceLocation"
    "fixture/CreateSourceLocation.yaml"

requestListPrefetchSchedules :: ListPrefetchSchedules -> TestTree
requestListPrefetchSchedules =
  req
    "ListPrefetchSchedules"
    "fixture/ListPrefetchSchedules.yaml"

requestDeletePrefetchSchedule :: DeletePrefetchSchedule -> TestTree
requestDeletePrefetchSchedule =
  req
    "DeletePrefetchSchedule"
    "fixture/DeletePrefetchSchedule.yaml"

requestListAlerts :: ListAlerts -> TestTree
requestListAlerts =
  req
    "ListAlerts"
    "fixture/ListAlerts.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestCreatePrefetchSchedule :: CreatePrefetchSchedule -> TestTree
requestCreatePrefetchSchedule =
  req
    "CreatePrefetchSchedule"
    "fixture/CreatePrefetchSchedule.yaml"

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

requestGetChannelPolicy :: GetChannelPolicy -> TestTree
requestGetChannelPolicy =
  req
    "GetChannelPolicy"
    "fixture/GetChannelPolicy.yaml"

requestDescribeVodSource :: DescribeVodSource -> TestTree
requestDescribeVodSource =
  req
    "DescribeVodSource"
    "fixture/DescribeVodSource.yaml"

requestDescribeSourceLocation :: DescribeSourceLocation -> TestTree
requestDescribeSourceLocation =
  req
    "DescribeSourceLocation"
    "fixture/DescribeSourceLocation.yaml"

requestGetPrefetchSchedule :: GetPrefetchSchedule -> TestTree
requestGetPrefetchSchedule =
  req
    "GetPrefetchSchedule"
    "fixture/GetPrefetchSchedule.yaml"

requestCreateProgram :: CreateProgram -> TestTree
requestCreateProgram =
  req
    "CreateProgram"
    "fixture/CreateProgram.yaml"

requestStartChannel :: StartChannel -> TestTree
requestStartChannel =
  req
    "StartChannel"
    "fixture/StartChannel.yaml"

requestListPlaybackConfigurations :: ListPlaybackConfigurations -> TestTree
requestListPlaybackConfigurations =
  req
    "ListPlaybackConfigurations"
    "fixture/ListPlaybackConfigurations.yaml"

requestDeletePlaybackConfiguration :: DeletePlaybackConfiguration -> TestTree
requestDeletePlaybackConfiguration =
  req
    "DeletePlaybackConfiguration"
    "fixture/DeletePlaybackConfiguration.yaml"

requestPutPlaybackConfiguration :: PutPlaybackConfiguration -> TestTree
requestPutPlaybackConfiguration =
  req
    "PutPlaybackConfiguration"
    "fixture/PutPlaybackConfiguration.yaml"

requestListSourceLocations :: ListSourceLocations -> TestTree
requestListSourceLocations =
  req
    "ListSourceLocations"
    "fixture/ListSourceLocations.yaml"

requestUpdateSourceLocation :: UpdateSourceLocation -> TestTree
requestUpdateSourceLocation =
  req
    "UpdateSourceLocation"
    "fixture/UpdateSourceLocation.yaml"

requestDeleteSourceLocation :: DeleteSourceLocation -> TestTree
requestDeleteSourceLocation =
  req
    "DeleteSourceLocation"
    "fixture/DeleteSourceLocation.yaml"

requestGetPlaybackConfiguration :: GetPlaybackConfiguration -> TestTree
requestGetPlaybackConfiguration =
  req
    "GetPlaybackConfiguration"
    "fixture/GetPlaybackConfiguration.yaml"

requestDeleteVodSource :: DeleteVodSource -> TestTree
requestDeleteVodSource =
  req
    "DeleteVodSource"
    "fixture/DeleteVodSource.yaml"

requestUpdateVodSource :: UpdateVodSource -> TestTree
requestUpdateVodSource =
  req
    "UpdateVodSource"
    "fixture/UpdateVodSource.yaml"

requestCreateVodSource :: CreateVodSource -> TestTree
requestCreateVodSource =
  req
    "CreateVodSource"
    "fixture/CreateVodSource.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDeleteChannelPolicy :: DeleteChannelPolicy -> TestTree
requestDeleteChannelPolicy =
  req
    "DeleteChannelPolicy"
    "fixture/DeleteChannelPolicy.yaml"

requestPutChannelPolicy :: PutChannelPolicy -> TestTree
requestPutChannelPolicy =
  req
    "PutChannelPolicy"
    "fixture/PutChannelPolicy.yaml"

requestDeleteProgram :: DeleteProgram -> TestTree
requestDeleteProgram =
  req
    "DeleteProgram"
    "fixture/DeleteProgram.yaml"

requestGetChannelSchedule :: GetChannelSchedule -> TestTree
requestGetChannelSchedule =
  req
    "GetChannelSchedule"
    "fixture/GetChannelSchedule.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestConfigureLogsForPlaybackConfiguration :: ConfigureLogsForPlaybackConfiguration -> TestTree
requestConfigureLogsForPlaybackConfiguration =
  req
    "ConfigureLogsForPlaybackConfiguration"
    "fixture/ConfigureLogsForPlaybackConfiguration.yaml"

requestStopChannel :: StopChannel -> TestTree
requestStopChannel =
  req
    "StopChannel"
    "fixture/StopChannel.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestListVodSources :: ListVodSources -> TestTree
requestListVodSources =
  req
    "ListVodSources"
    "fixture/ListVodSources.yaml"

requestDescribeProgram :: DescribeProgram -> TestTree
requestDescribeProgram =
  req
    "DescribeProgram"
    "fixture/DescribeProgram.yaml"

-- Responses

responseCreateSourceLocation :: CreateSourceLocationResponse -> TestTree
responseCreateSourceLocation =
  res
    "CreateSourceLocationResponse"
    "fixture/CreateSourceLocationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSourceLocation)

responseListPrefetchSchedules :: ListPrefetchSchedulesResponse -> TestTree
responseListPrefetchSchedules =
  res
    "ListPrefetchSchedulesResponse"
    "fixture/ListPrefetchSchedulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPrefetchSchedules)

responseDeletePrefetchSchedule :: DeletePrefetchScheduleResponse -> TestTree
responseDeletePrefetchSchedule =
  res
    "DeletePrefetchScheduleResponse"
    "fixture/DeletePrefetchScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePrefetchSchedule)

responseListAlerts :: ListAlertsResponse -> TestTree
responseListAlerts =
  res
    "ListAlertsResponse"
    "fixture/ListAlertsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAlerts)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListChannels)

responseCreatePrefetchSchedule :: CreatePrefetchScheduleResponse -> TestTree
responseCreatePrefetchSchedule =
  res
    "CreatePrefetchScheduleResponse"
    "fixture/CreatePrefetchScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePrefetchSchedule)

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
    (Proxy :: Proxy UpdateChannel)

responseGetChannelPolicy :: GetChannelPolicyResponse -> TestTree
responseGetChannelPolicy =
  res
    "GetChannelPolicyResponse"
    "fixture/GetChannelPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetChannelPolicy)

responseDescribeVodSource :: DescribeVodSourceResponse -> TestTree
responseDescribeVodSource =
  res
    "DescribeVodSourceResponse"
    "fixture/DescribeVodSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeVodSource)

responseDescribeSourceLocation :: DescribeSourceLocationResponse -> TestTree
responseDescribeSourceLocation =
  res
    "DescribeSourceLocationResponse"
    "fixture/DescribeSourceLocationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSourceLocation)

responseGetPrefetchSchedule :: GetPrefetchScheduleResponse -> TestTree
responseGetPrefetchSchedule =
  res
    "GetPrefetchScheduleResponse"
    "fixture/GetPrefetchScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy GetPrefetchSchedule)

responseCreateProgram :: CreateProgramResponse -> TestTree
responseCreateProgram =
  res
    "CreateProgramResponse"
    "fixture/CreateProgramResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProgram)

responseStartChannel :: StartChannelResponse -> TestTree
responseStartChannel =
  res
    "StartChannelResponse"
    "fixture/StartChannelResponse.proto"
    defaultService
    (Proxy :: Proxy StartChannel)

responseListPlaybackConfigurations :: ListPlaybackConfigurationsResponse -> TestTree
responseListPlaybackConfigurations =
  res
    "ListPlaybackConfigurationsResponse"
    "fixture/ListPlaybackConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPlaybackConfigurations)

responseDeletePlaybackConfiguration :: DeletePlaybackConfigurationResponse -> TestTree
responseDeletePlaybackConfiguration =
  res
    "DeletePlaybackConfigurationResponse"
    "fixture/DeletePlaybackConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePlaybackConfiguration)

responsePutPlaybackConfiguration :: PutPlaybackConfigurationResponse -> TestTree
responsePutPlaybackConfiguration =
  res
    "PutPlaybackConfigurationResponse"
    "fixture/PutPlaybackConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutPlaybackConfiguration)

responseListSourceLocations :: ListSourceLocationsResponse -> TestTree
responseListSourceLocations =
  res
    "ListSourceLocationsResponse"
    "fixture/ListSourceLocationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSourceLocations)

responseUpdateSourceLocation :: UpdateSourceLocationResponse -> TestTree
responseUpdateSourceLocation =
  res
    "UpdateSourceLocationResponse"
    "fixture/UpdateSourceLocationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSourceLocation)

responseDeleteSourceLocation :: DeleteSourceLocationResponse -> TestTree
responseDeleteSourceLocation =
  res
    "DeleteSourceLocationResponse"
    "fixture/DeleteSourceLocationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSourceLocation)

responseGetPlaybackConfiguration :: GetPlaybackConfigurationResponse -> TestTree
responseGetPlaybackConfiguration =
  res
    "GetPlaybackConfigurationResponse"
    "fixture/GetPlaybackConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetPlaybackConfiguration)

responseDeleteVodSource :: DeleteVodSourceResponse -> TestTree
responseDeleteVodSource =
  res
    "DeleteVodSourceResponse"
    "fixture/DeleteVodSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVodSource)

responseUpdateVodSource :: UpdateVodSourceResponse -> TestTree
responseUpdateVodSource =
  res
    "UpdateVodSourceResponse"
    "fixture/UpdateVodSourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVodSource)

responseCreateVodSource :: CreateVodSourceResponse -> TestTree
responseCreateVodSource =
  res
    "CreateVodSourceResponse"
    "fixture/CreateVodSourceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVodSource)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChannel)

responseDeleteChannelPolicy :: DeleteChannelPolicyResponse -> TestTree
responseDeleteChannelPolicy =
  res
    "DeleteChannelPolicyResponse"
    "fixture/DeleteChannelPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteChannelPolicy)

responsePutChannelPolicy :: PutChannelPolicyResponse -> TestTree
responsePutChannelPolicy =
  res
    "PutChannelPolicyResponse"
    "fixture/PutChannelPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutChannelPolicy)

responseDeleteProgram :: DeleteProgramResponse -> TestTree
responseDeleteProgram =
  res
    "DeleteProgramResponse"
    "fixture/DeleteProgramResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProgram)

responseGetChannelSchedule :: GetChannelScheduleResponse -> TestTree
responseGetChannelSchedule =
  res
    "GetChannelScheduleResponse"
    "fixture/GetChannelScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy GetChannelSchedule)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseConfigureLogsForPlaybackConfiguration :: ConfigureLogsForPlaybackConfigurationResponse -> TestTree
responseConfigureLogsForPlaybackConfiguration =
  res
    "ConfigureLogsForPlaybackConfigurationResponse"
    "fixture/ConfigureLogsForPlaybackConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy ConfigureLogsForPlaybackConfiguration)

responseStopChannel :: StopChannelResponse -> TestTree
responseStopChannel =
  res
    "StopChannelResponse"
    "fixture/StopChannelResponse.proto"
    defaultService
    (Proxy :: Proxy StopChannel)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeChannel)

responseListVodSources :: ListVodSourcesResponse -> TestTree
responseListVodSources =
  res
    "ListVodSourcesResponse"
    "fixture/ListVodSourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListVodSources)

responseDescribeProgram :: DescribeProgramResponse -> TestTree
responseDescribeProgram =
  res
    "DescribeProgramResponse"
    "fixture/DescribeProgramResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProgram)
