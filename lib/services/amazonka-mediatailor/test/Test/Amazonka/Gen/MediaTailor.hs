{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MediaTailor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MediaTailor where

import Amazonka.MediaTailor
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MediaTailor.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestConfigureLogsForPlaybackConfiguration $
--             newConfigureLogsForPlaybackConfiguration
--
--         , requestCreateChannel $
--             newCreateChannel
--
--         , requestCreateLiveSource $
--             newCreateLiveSource
--
--         , requestCreatePrefetchSchedule $
--             newCreatePrefetchSchedule
--
--         , requestCreateProgram $
--             newCreateProgram
--
--         , requestCreateSourceLocation $
--             newCreateSourceLocation
--
--         , requestCreateVodSource $
--             newCreateVodSource
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestDeleteChannelPolicy $
--             newDeleteChannelPolicy
--
--         , requestDeleteLiveSource $
--             newDeleteLiveSource
--
--         , requestDeletePlaybackConfiguration $
--             newDeletePlaybackConfiguration
--
--         , requestDeletePrefetchSchedule $
--             newDeletePrefetchSchedule
--
--         , requestDeleteProgram $
--             newDeleteProgram
--
--         , requestDeleteSourceLocation $
--             newDeleteSourceLocation
--
--         , requestDeleteVodSource $
--             newDeleteVodSource
--
--         , requestDescribeChannel $
--             newDescribeChannel
--
--         , requestDescribeLiveSource $
--             newDescribeLiveSource
--
--         , requestDescribeProgram $
--             newDescribeProgram
--
--         , requestDescribeSourceLocation $
--             newDescribeSourceLocation
--
--         , requestDescribeVodSource $
--             newDescribeVodSource
--
--         , requestGetChannelPolicy $
--             newGetChannelPolicy
--
--         , requestGetChannelSchedule $
--             newGetChannelSchedule
--
--         , requestGetPlaybackConfiguration $
--             newGetPlaybackConfiguration
--
--         , requestGetPrefetchSchedule $
--             newGetPrefetchSchedule
--
--         , requestListAlerts $
--             newListAlerts
--
--         , requestListChannels $
--             newListChannels
--
--         , requestListLiveSources $
--             newListLiveSources
--
--         , requestListPlaybackConfigurations $
--             newListPlaybackConfigurations
--
--         , requestListPrefetchSchedules $
--             newListPrefetchSchedules
--
--         , requestListSourceLocations $
--             newListSourceLocations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListVodSources $
--             newListVodSources
--
--         , requestPutChannelPolicy $
--             newPutChannelPolicy
--
--         , requestPutPlaybackConfiguration $
--             newPutPlaybackConfiguration
--
--         , requestStartChannel $
--             newStartChannel
--
--         , requestStopChannel $
--             newStopChannel
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateChannel $
--             newUpdateChannel
--
--         , requestUpdateLiveSource $
--             newUpdateLiveSource
--
--         , requestUpdateSourceLocation $
--             newUpdateSourceLocation
--
--         , requestUpdateVodSource $
--             newUpdateVodSource
--
--           ]

--     , testGroup "response"
--         [ responseConfigureLogsForPlaybackConfiguration $
--             newConfigureLogsForPlaybackConfigurationResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseCreateLiveSource $
--             newCreateLiveSourceResponse
--
--         , responseCreatePrefetchSchedule $
--             newCreatePrefetchScheduleResponse
--
--         , responseCreateProgram $
--             newCreateProgramResponse
--
--         , responseCreateSourceLocation $
--             newCreateSourceLocationResponse
--
--         , responseCreateVodSource $
--             newCreateVodSourceResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseDeleteChannelPolicy $
--             newDeleteChannelPolicyResponse
--
--         , responseDeleteLiveSource $
--             newDeleteLiveSourceResponse
--
--         , responseDeletePlaybackConfiguration $
--             newDeletePlaybackConfigurationResponse
--
--         , responseDeletePrefetchSchedule $
--             newDeletePrefetchScheduleResponse
--
--         , responseDeleteProgram $
--             newDeleteProgramResponse
--
--         , responseDeleteSourceLocation $
--             newDeleteSourceLocationResponse
--
--         , responseDeleteVodSource $
--             newDeleteVodSourceResponse
--
--         , responseDescribeChannel $
--             newDescribeChannelResponse
--
--         , responseDescribeLiveSource $
--             newDescribeLiveSourceResponse
--
--         , responseDescribeProgram $
--             newDescribeProgramResponse
--
--         , responseDescribeSourceLocation $
--             newDescribeSourceLocationResponse
--
--         , responseDescribeVodSource $
--             newDescribeVodSourceResponse
--
--         , responseGetChannelPolicy $
--             newGetChannelPolicyResponse
--
--         , responseGetChannelSchedule $
--             newGetChannelScheduleResponse
--
--         , responseGetPlaybackConfiguration $
--             newGetPlaybackConfigurationResponse
--
--         , responseGetPrefetchSchedule $
--             newGetPrefetchScheduleResponse
--
--         , responseListAlerts $
--             newListAlertsResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseListLiveSources $
--             newListLiveSourcesResponse
--
--         , responseListPlaybackConfigurations $
--             newListPlaybackConfigurationsResponse
--
--         , responseListPrefetchSchedules $
--             newListPrefetchSchedulesResponse
--
--         , responseListSourceLocations $
--             newListSourceLocationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListVodSources $
--             newListVodSourcesResponse
--
--         , responsePutChannelPolicy $
--             newPutChannelPolicyResponse
--
--         , responsePutPlaybackConfiguration $
--             newPutPlaybackConfigurationResponse
--
--         , responseStartChannel $
--             newStartChannelResponse
--
--         , responseStopChannel $
--             newStopChannelResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateChannel $
--             newUpdateChannelResponse
--
--         , responseUpdateLiveSource $
--             newUpdateLiveSourceResponse
--
--         , responseUpdateSourceLocation $
--             newUpdateSourceLocationResponse
--
--         , responseUpdateVodSource $
--             newUpdateVodSourceResponse
--
--           ]
--     ]

-- Requests

requestConfigureLogsForPlaybackConfiguration :: ConfigureLogsForPlaybackConfiguration -> TestTree
requestConfigureLogsForPlaybackConfiguration =
  req
    "ConfigureLogsForPlaybackConfiguration"
    "fixture/ConfigureLogsForPlaybackConfiguration.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestCreateLiveSource :: CreateLiveSource -> TestTree
requestCreateLiveSource =
  req
    "CreateLiveSource"
    "fixture/CreateLiveSource.yaml"

requestCreatePrefetchSchedule :: CreatePrefetchSchedule -> TestTree
requestCreatePrefetchSchedule =
  req
    "CreatePrefetchSchedule"
    "fixture/CreatePrefetchSchedule.yaml"

requestCreateProgram :: CreateProgram -> TestTree
requestCreateProgram =
  req
    "CreateProgram"
    "fixture/CreateProgram.yaml"

requestCreateSourceLocation :: CreateSourceLocation -> TestTree
requestCreateSourceLocation =
  req
    "CreateSourceLocation"
    "fixture/CreateSourceLocation.yaml"

requestCreateVodSource :: CreateVodSource -> TestTree
requestCreateVodSource =
  req
    "CreateVodSource"
    "fixture/CreateVodSource.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestDeleteChannelPolicy :: DeleteChannelPolicy -> TestTree
requestDeleteChannelPolicy =
  req
    "DeleteChannelPolicy"
    "fixture/DeleteChannelPolicy.yaml"

requestDeleteLiveSource :: DeleteLiveSource -> TestTree
requestDeleteLiveSource =
  req
    "DeleteLiveSource"
    "fixture/DeleteLiveSource.yaml"

requestDeletePlaybackConfiguration :: DeletePlaybackConfiguration -> TestTree
requestDeletePlaybackConfiguration =
  req
    "DeletePlaybackConfiguration"
    "fixture/DeletePlaybackConfiguration.yaml"

requestDeletePrefetchSchedule :: DeletePrefetchSchedule -> TestTree
requestDeletePrefetchSchedule =
  req
    "DeletePrefetchSchedule"
    "fixture/DeletePrefetchSchedule.yaml"

requestDeleteProgram :: DeleteProgram -> TestTree
requestDeleteProgram =
  req
    "DeleteProgram"
    "fixture/DeleteProgram.yaml"

requestDeleteSourceLocation :: DeleteSourceLocation -> TestTree
requestDeleteSourceLocation =
  req
    "DeleteSourceLocation"
    "fixture/DeleteSourceLocation.yaml"

requestDeleteVodSource :: DeleteVodSource -> TestTree
requestDeleteVodSource =
  req
    "DeleteVodSource"
    "fixture/DeleteVodSource.yaml"

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestDescribeLiveSource :: DescribeLiveSource -> TestTree
requestDescribeLiveSource =
  req
    "DescribeLiveSource"
    "fixture/DescribeLiveSource.yaml"

requestDescribeProgram :: DescribeProgram -> TestTree
requestDescribeProgram =
  req
    "DescribeProgram"
    "fixture/DescribeProgram.yaml"

requestDescribeSourceLocation :: DescribeSourceLocation -> TestTree
requestDescribeSourceLocation =
  req
    "DescribeSourceLocation"
    "fixture/DescribeSourceLocation.yaml"

requestDescribeVodSource :: DescribeVodSource -> TestTree
requestDescribeVodSource =
  req
    "DescribeVodSource"
    "fixture/DescribeVodSource.yaml"

requestGetChannelPolicy :: GetChannelPolicy -> TestTree
requestGetChannelPolicy =
  req
    "GetChannelPolicy"
    "fixture/GetChannelPolicy.yaml"

requestGetChannelSchedule :: GetChannelSchedule -> TestTree
requestGetChannelSchedule =
  req
    "GetChannelSchedule"
    "fixture/GetChannelSchedule.yaml"

requestGetPlaybackConfiguration :: GetPlaybackConfiguration -> TestTree
requestGetPlaybackConfiguration =
  req
    "GetPlaybackConfiguration"
    "fixture/GetPlaybackConfiguration.yaml"

requestGetPrefetchSchedule :: GetPrefetchSchedule -> TestTree
requestGetPrefetchSchedule =
  req
    "GetPrefetchSchedule"
    "fixture/GetPrefetchSchedule.yaml"

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

requestListLiveSources :: ListLiveSources -> TestTree
requestListLiveSources =
  req
    "ListLiveSources"
    "fixture/ListLiveSources.yaml"

requestListPlaybackConfigurations :: ListPlaybackConfigurations -> TestTree
requestListPlaybackConfigurations =
  req
    "ListPlaybackConfigurations"
    "fixture/ListPlaybackConfigurations.yaml"

requestListPrefetchSchedules :: ListPrefetchSchedules -> TestTree
requestListPrefetchSchedules =
  req
    "ListPrefetchSchedules"
    "fixture/ListPrefetchSchedules.yaml"

requestListSourceLocations :: ListSourceLocations -> TestTree
requestListSourceLocations =
  req
    "ListSourceLocations"
    "fixture/ListSourceLocations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListVodSources :: ListVodSources -> TestTree
requestListVodSources =
  req
    "ListVodSources"
    "fixture/ListVodSources.yaml"

requestPutChannelPolicy :: PutChannelPolicy -> TestTree
requestPutChannelPolicy =
  req
    "PutChannelPolicy"
    "fixture/PutChannelPolicy.yaml"

requestPutPlaybackConfiguration :: PutPlaybackConfiguration -> TestTree
requestPutPlaybackConfiguration =
  req
    "PutPlaybackConfiguration"
    "fixture/PutPlaybackConfiguration.yaml"

requestStartChannel :: StartChannel -> TestTree
requestStartChannel =
  req
    "StartChannel"
    "fixture/StartChannel.yaml"

requestStopChannel :: StopChannel -> TestTree
requestStopChannel =
  req
    "StopChannel"
    "fixture/StopChannel.yaml"

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

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestUpdateLiveSource :: UpdateLiveSource -> TestTree
requestUpdateLiveSource =
  req
    "UpdateLiveSource"
    "fixture/UpdateLiveSource.yaml"

requestUpdateSourceLocation :: UpdateSourceLocation -> TestTree
requestUpdateSourceLocation =
  req
    "UpdateSourceLocation"
    "fixture/UpdateSourceLocation.yaml"

requestUpdateVodSource :: UpdateVodSource -> TestTree
requestUpdateVodSource =
  req
    "UpdateVodSource"
    "fixture/UpdateVodSource.yaml"

-- Responses

responseConfigureLogsForPlaybackConfiguration :: ConfigureLogsForPlaybackConfigurationResponse -> TestTree
responseConfigureLogsForPlaybackConfiguration =
  res
    "ConfigureLogsForPlaybackConfigurationResponse"
    "fixture/ConfigureLogsForPlaybackConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfigureLogsForPlaybackConfiguration)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel)

responseCreateLiveSource :: CreateLiveSourceResponse -> TestTree
responseCreateLiveSource =
  res
    "CreateLiveSourceResponse"
    "fixture/CreateLiveSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLiveSource)

responseCreatePrefetchSchedule :: CreatePrefetchScheduleResponse -> TestTree
responseCreatePrefetchSchedule =
  res
    "CreatePrefetchScheduleResponse"
    "fixture/CreatePrefetchScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePrefetchSchedule)

responseCreateProgram :: CreateProgramResponse -> TestTree
responseCreateProgram =
  res
    "CreateProgramResponse"
    "fixture/CreateProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProgram)

responseCreateSourceLocation :: CreateSourceLocationResponse -> TestTree
responseCreateSourceLocation =
  res
    "CreateSourceLocationResponse"
    "fixture/CreateSourceLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSourceLocation)

responseCreateVodSource :: CreateVodSourceResponse -> TestTree
responseCreateVodSource =
  res
    "CreateVodSourceResponse"
    "fixture/CreateVodSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVodSource)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannel)

responseDeleteChannelPolicy :: DeleteChannelPolicyResponse -> TestTree
responseDeleteChannelPolicy =
  res
    "DeleteChannelPolicyResponse"
    "fixture/DeleteChannelPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannelPolicy)

responseDeleteLiveSource :: DeleteLiveSourceResponse -> TestTree
responseDeleteLiveSource =
  res
    "DeleteLiveSourceResponse"
    "fixture/DeleteLiveSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLiveSource)

responseDeletePlaybackConfiguration :: DeletePlaybackConfigurationResponse -> TestTree
responseDeletePlaybackConfiguration =
  res
    "DeletePlaybackConfigurationResponse"
    "fixture/DeletePlaybackConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlaybackConfiguration)

responseDeletePrefetchSchedule :: DeletePrefetchScheduleResponse -> TestTree
responseDeletePrefetchSchedule =
  res
    "DeletePrefetchScheduleResponse"
    "fixture/DeletePrefetchScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePrefetchSchedule)

responseDeleteProgram :: DeleteProgramResponse -> TestTree
responseDeleteProgram =
  res
    "DeleteProgramResponse"
    "fixture/DeleteProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProgram)

responseDeleteSourceLocation :: DeleteSourceLocationResponse -> TestTree
responseDeleteSourceLocation =
  res
    "DeleteSourceLocationResponse"
    "fixture/DeleteSourceLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSourceLocation)

responseDeleteVodSource :: DeleteVodSourceResponse -> TestTree
responseDeleteVodSource =
  res
    "DeleteVodSourceResponse"
    "fixture/DeleteVodSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVodSource)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeChannel)

responseDescribeLiveSource :: DescribeLiveSourceResponse -> TestTree
responseDescribeLiveSource =
  res
    "DescribeLiveSourceResponse"
    "fixture/DescribeLiveSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLiveSource)

responseDescribeProgram :: DescribeProgramResponse -> TestTree
responseDescribeProgram =
  res
    "DescribeProgramResponse"
    "fixture/DescribeProgramResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProgram)

responseDescribeSourceLocation :: DescribeSourceLocationResponse -> TestTree
responseDescribeSourceLocation =
  res
    "DescribeSourceLocationResponse"
    "fixture/DescribeSourceLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSourceLocation)

responseDescribeVodSource :: DescribeVodSourceResponse -> TestTree
responseDescribeVodSource =
  res
    "DescribeVodSourceResponse"
    "fixture/DescribeVodSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVodSource)

responseGetChannelPolicy :: GetChannelPolicyResponse -> TestTree
responseGetChannelPolicy =
  res
    "GetChannelPolicyResponse"
    "fixture/GetChannelPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelPolicy)

responseGetChannelSchedule :: GetChannelScheduleResponse -> TestTree
responseGetChannelSchedule =
  res
    "GetChannelScheduleResponse"
    "fixture/GetChannelScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannelSchedule)

responseGetPlaybackConfiguration :: GetPlaybackConfigurationResponse -> TestTree
responseGetPlaybackConfiguration =
  res
    "GetPlaybackConfigurationResponse"
    "fixture/GetPlaybackConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPlaybackConfiguration)

responseGetPrefetchSchedule :: GetPrefetchScheduleResponse -> TestTree
responseGetPrefetchSchedule =
  res
    "GetPrefetchScheduleResponse"
    "fixture/GetPrefetchScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPrefetchSchedule)

responseListAlerts :: ListAlertsResponse -> TestTree
responseListAlerts =
  res
    "ListAlertsResponse"
    "fixture/ListAlertsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlerts)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannels)

responseListLiveSources :: ListLiveSourcesResponse -> TestTree
responseListLiveSources =
  res
    "ListLiveSourcesResponse"
    "fixture/ListLiveSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLiveSources)

responseListPlaybackConfigurations :: ListPlaybackConfigurationsResponse -> TestTree
responseListPlaybackConfigurations =
  res
    "ListPlaybackConfigurationsResponse"
    "fixture/ListPlaybackConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlaybackConfigurations)

responseListPrefetchSchedules :: ListPrefetchSchedulesResponse -> TestTree
responseListPrefetchSchedules =
  res
    "ListPrefetchSchedulesResponse"
    "fixture/ListPrefetchSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrefetchSchedules)

responseListSourceLocations :: ListSourceLocationsResponse -> TestTree
responseListSourceLocations =
  res
    "ListSourceLocationsResponse"
    "fixture/ListSourceLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSourceLocations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListVodSources :: ListVodSourcesResponse -> TestTree
responseListVodSources =
  res
    "ListVodSourcesResponse"
    "fixture/ListVodSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVodSources)

responsePutChannelPolicy :: PutChannelPolicyResponse -> TestTree
responsePutChannelPolicy =
  res
    "PutChannelPolicyResponse"
    "fixture/PutChannelPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutChannelPolicy)

responsePutPlaybackConfiguration :: PutPlaybackConfigurationResponse -> TestTree
responsePutPlaybackConfiguration =
  res
    "PutPlaybackConfigurationResponse"
    "fixture/PutPlaybackConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPlaybackConfiguration)

responseStartChannel :: StartChannelResponse -> TestTree
responseStartChannel =
  res
    "StartChannelResponse"
    "fixture/StartChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartChannel)

responseStopChannel :: StopChannelResponse -> TestTree
responseStopChannel =
  res
    "StopChannelResponse"
    "fixture/StopChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopChannel)

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

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChannel)

responseUpdateLiveSource :: UpdateLiveSourceResponse -> TestTree
responseUpdateLiveSource =
  res
    "UpdateLiveSourceResponse"
    "fixture/UpdateLiveSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLiveSource)

responseUpdateSourceLocation :: UpdateSourceLocationResponse -> TestTree
responseUpdateSourceLocation =
  res
    "UpdateSourceLocationResponse"
    "fixture/UpdateSourceLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSourceLocation)

responseUpdateVodSource :: UpdateVodSourceResponse -> TestTree
responseUpdateVodSource =
  res
    "UpdateVodSourceResponse"
    "fixture/UpdateVodSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVodSource)
