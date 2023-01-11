{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IVS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IVS where

import Amazonka.IVS
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IVS.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchGetChannel $
--             newBatchGetChannel
--
--         , requestBatchGetStreamKey $
--             newBatchGetStreamKey
--
--         , requestCreateChannel $
--             newCreateChannel
--
--         , requestCreateRecordingConfiguration $
--             newCreateRecordingConfiguration
--
--         , requestCreateStreamKey $
--             newCreateStreamKey
--
--         , requestDeleteChannel $
--             newDeleteChannel
--
--         , requestDeletePlaybackKeyPair $
--             newDeletePlaybackKeyPair
--
--         , requestDeleteRecordingConfiguration $
--             newDeleteRecordingConfiguration
--
--         , requestDeleteStreamKey $
--             newDeleteStreamKey
--
--         , requestGetChannel $
--             newGetChannel
--
--         , requestGetPlaybackKeyPair $
--             newGetPlaybackKeyPair
--
--         , requestGetRecordingConfiguration $
--             newGetRecordingConfiguration
--
--         , requestGetStream $
--             newGetStream
--
--         , requestGetStreamKey $
--             newGetStreamKey
--
--         , requestGetStreamSession $
--             newGetStreamSession
--
--         , requestImportPlaybackKeyPair $
--             newImportPlaybackKeyPair
--
--         , requestListChannels $
--             newListChannels
--
--         , requestListPlaybackKeyPairs $
--             newListPlaybackKeyPairs
--
--         , requestListRecordingConfigurations $
--             newListRecordingConfigurations
--
--         , requestListStreamKeys $
--             newListStreamKeys
--
--         , requestListStreamSessions $
--             newListStreamSessions
--
--         , requestListStreams $
--             newListStreams
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutMetadata $
--             newPutMetadata
--
--         , requestStopStream $
--             newStopStream
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
--           ]

--     , testGroup "response"
--         [ responseBatchGetChannel $
--             newBatchGetChannelResponse
--
--         , responseBatchGetStreamKey $
--             newBatchGetStreamKeyResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseCreateRecordingConfiguration $
--             newCreateRecordingConfigurationResponse
--
--         , responseCreateStreamKey $
--             newCreateStreamKeyResponse
--
--         , responseDeleteChannel $
--             newDeleteChannelResponse
--
--         , responseDeletePlaybackKeyPair $
--             newDeletePlaybackKeyPairResponse
--
--         , responseDeleteRecordingConfiguration $
--             newDeleteRecordingConfigurationResponse
--
--         , responseDeleteStreamKey $
--             newDeleteStreamKeyResponse
--
--         , responseGetChannel $
--             newGetChannelResponse
--
--         , responseGetPlaybackKeyPair $
--             newGetPlaybackKeyPairResponse
--
--         , responseGetRecordingConfiguration $
--             newGetRecordingConfigurationResponse
--
--         , responseGetStream $
--             newGetStreamResponse
--
--         , responseGetStreamKey $
--             newGetStreamKeyResponse
--
--         , responseGetStreamSession $
--             newGetStreamSessionResponse
--
--         , responseImportPlaybackKeyPair $
--             newImportPlaybackKeyPairResponse
--
--         , responseListChannels $
--             newListChannelsResponse
--
--         , responseListPlaybackKeyPairs $
--             newListPlaybackKeyPairsResponse
--
--         , responseListRecordingConfigurations $
--             newListRecordingConfigurationsResponse
--
--         , responseListStreamKeys $
--             newListStreamKeysResponse
--
--         , responseListStreamSessions $
--             newListStreamSessionsResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutMetadata $
--             newPutMetadataResponse
--
--         , responseStopStream $
--             newStopStreamResponse
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
--           ]
--     ]

-- Requests

requestBatchGetChannel :: BatchGetChannel -> TestTree
requestBatchGetChannel =
  req
    "BatchGetChannel"
    "fixture/BatchGetChannel.yaml"

requestBatchGetStreamKey :: BatchGetStreamKey -> TestTree
requestBatchGetStreamKey =
  req
    "BatchGetStreamKey"
    "fixture/BatchGetStreamKey.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestCreateRecordingConfiguration :: CreateRecordingConfiguration -> TestTree
requestCreateRecordingConfiguration =
  req
    "CreateRecordingConfiguration"
    "fixture/CreateRecordingConfiguration.yaml"

requestCreateStreamKey :: CreateStreamKey -> TestTree
requestCreateStreamKey =
  req
    "CreateStreamKey"
    "fixture/CreateStreamKey.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestDeletePlaybackKeyPair :: DeletePlaybackKeyPair -> TestTree
requestDeletePlaybackKeyPair =
  req
    "DeletePlaybackKeyPair"
    "fixture/DeletePlaybackKeyPair.yaml"

requestDeleteRecordingConfiguration :: DeleteRecordingConfiguration -> TestTree
requestDeleteRecordingConfiguration =
  req
    "DeleteRecordingConfiguration"
    "fixture/DeleteRecordingConfiguration.yaml"

requestDeleteStreamKey :: DeleteStreamKey -> TestTree
requestDeleteStreamKey =
  req
    "DeleteStreamKey"
    "fixture/DeleteStreamKey.yaml"

requestGetChannel :: GetChannel -> TestTree
requestGetChannel =
  req
    "GetChannel"
    "fixture/GetChannel.yaml"

requestGetPlaybackKeyPair :: GetPlaybackKeyPair -> TestTree
requestGetPlaybackKeyPair =
  req
    "GetPlaybackKeyPair"
    "fixture/GetPlaybackKeyPair.yaml"

requestGetRecordingConfiguration :: GetRecordingConfiguration -> TestTree
requestGetRecordingConfiguration =
  req
    "GetRecordingConfiguration"
    "fixture/GetRecordingConfiguration.yaml"

requestGetStream :: GetStream -> TestTree
requestGetStream =
  req
    "GetStream"
    "fixture/GetStream.yaml"

requestGetStreamKey :: GetStreamKey -> TestTree
requestGetStreamKey =
  req
    "GetStreamKey"
    "fixture/GetStreamKey.yaml"

requestGetStreamSession :: GetStreamSession -> TestTree
requestGetStreamSession =
  req
    "GetStreamSession"
    "fixture/GetStreamSession.yaml"

requestImportPlaybackKeyPair :: ImportPlaybackKeyPair -> TestTree
requestImportPlaybackKeyPair =
  req
    "ImportPlaybackKeyPair"
    "fixture/ImportPlaybackKeyPair.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestListPlaybackKeyPairs :: ListPlaybackKeyPairs -> TestTree
requestListPlaybackKeyPairs =
  req
    "ListPlaybackKeyPairs"
    "fixture/ListPlaybackKeyPairs.yaml"

requestListRecordingConfigurations :: ListRecordingConfigurations -> TestTree
requestListRecordingConfigurations =
  req
    "ListRecordingConfigurations"
    "fixture/ListRecordingConfigurations.yaml"

requestListStreamKeys :: ListStreamKeys -> TestTree
requestListStreamKeys =
  req
    "ListStreamKeys"
    "fixture/ListStreamKeys.yaml"

requestListStreamSessions :: ListStreamSessions -> TestTree
requestListStreamSessions =
  req
    "ListStreamSessions"
    "fixture/ListStreamSessions.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutMetadata :: PutMetadata -> TestTree
requestPutMetadata =
  req
    "PutMetadata"
    "fixture/PutMetadata.yaml"

requestStopStream :: StopStream -> TestTree
requestStopStream =
  req
    "StopStream"
    "fixture/StopStream.yaml"

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

-- Responses

responseBatchGetChannel :: BatchGetChannelResponse -> TestTree
responseBatchGetChannel =
  res
    "BatchGetChannelResponse"
    "fixture/BatchGetChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetChannel)

responseBatchGetStreamKey :: BatchGetStreamKeyResponse -> TestTree
responseBatchGetStreamKey =
  res
    "BatchGetStreamKeyResponse"
    "fixture/BatchGetStreamKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetStreamKey)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel)

responseCreateRecordingConfiguration :: CreateRecordingConfigurationResponse -> TestTree
responseCreateRecordingConfiguration =
  res
    "CreateRecordingConfigurationResponse"
    "fixture/CreateRecordingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecordingConfiguration)

responseCreateStreamKey :: CreateStreamKeyResponse -> TestTree
responseCreateStreamKey =
  res
    "CreateStreamKeyResponse"
    "fixture/CreateStreamKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamKey)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteChannel)

responseDeletePlaybackKeyPair :: DeletePlaybackKeyPairResponse -> TestTree
responseDeletePlaybackKeyPair =
  res
    "DeletePlaybackKeyPairResponse"
    "fixture/DeletePlaybackKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlaybackKeyPair)

responseDeleteRecordingConfiguration :: DeleteRecordingConfigurationResponse -> TestTree
responseDeleteRecordingConfiguration =
  res
    "DeleteRecordingConfigurationResponse"
    "fixture/DeleteRecordingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecordingConfiguration)

responseDeleteStreamKey :: DeleteStreamKeyResponse -> TestTree
responseDeleteStreamKey =
  res
    "DeleteStreamKeyResponse"
    "fixture/DeleteStreamKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamKey)

responseGetChannel :: GetChannelResponse -> TestTree
responseGetChannel =
  res
    "GetChannelResponse"
    "fixture/GetChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannel)

responseGetPlaybackKeyPair :: GetPlaybackKeyPairResponse -> TestTree
responseGetPlaybackKeyPair =
  res
    "GetPlaybackKeyPairResponse"
    "fixture/GetPlaybackKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPlaybackKeyPair)

responseGetRecordingConfiguration :: GetRecordingConfigurationResponse -> TestTree
responseGetRecordingConfiguration =
  res
    "GetRecordingConfigurationResponse"
    "fixture/GetRecordingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecordingConfiguration)

responseGetStream :: GetStreamResponse -> TestTree
responseGetStream =
  res
    "GetStreamResponse"
    "fixture/GetStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStream)

responseGetStreamKey :: GetStreamKeyResponse -> TestTree
responseGetStreamKey =
  res
    "GetStreamKeyResponse"
    "fixture/GetStreamKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamKey)

responseGetStreamSession :: GetStreamSessionResponse -> TestTree
responseGetStreamSession =
  res
    "GetStreamSessionResponse"
    "fixture/GetStreamSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamSession)

responseImportPlaybackKeyPair :: ImportPlaybackKeyPairResponse -> TestTree
responseImportPlaybackKeyPair =
  res
    "ImportPlaybackKeyPairResponse"
    "fixture/ImportPlaybackKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportPlaybackKeyPair)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChannels)

responseListPlaybackKeyPairs :: ListPlaybackKeyPairsResponse -> TestTree
responseListPlaybackKeyPairs =
  res
    "ListPlaybackKeyPairsResponse"
    "fixture/ListPlaybackKeyPairsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlaybackKeyPairs)

responseListRecordingConfigurations :: ListRecordingConfigurationsResponse -> TestTree
responseListRecordingConfigurations =
  res
    "ListRecordingConfigurationsResponse"
    "fixture/ListRecordingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecordingConfigurations)

responseListStreamKeys :: ListStreamKeysResponse -> TestTree
responseListStreamKeys =
  res
    "ListStreamKeysResponse"
    "fixture/ListStreamKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamKeys)

responseListStreamSessions :: ListStreamSessionsResponse -> TestTree
responseListStreamSessions =
  res
    "ListStreamSessionsResponse"
    "fixture/ListStreamSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamSessions)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreams)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutMetadata :: PutMetadataResponse -> TestTree
responsePutMetadata =
  res
    "PutMetadataResponse"
    "fixture/PutMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMetadata)

responseStopStream :: StopStreamResponse -> TestTree
responseStopStream =
  res
    "StopStreamResponse"
    "fixture/StopStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStream)

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
