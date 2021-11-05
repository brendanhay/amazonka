{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IVS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IVS where

import qualified Data.Proxy as Proxy
import Network.AWS.IVS
import Test.AWS.Fixture
import Test.AWS.IVS.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutMetadata $
--             newPutMetadata
--
--         , requestListRecordingConfigurations $
--             newListRecordingConfigurations
--
--         , requestCreateStreamKey $
--             newCreateStreamKey
--
--         , requestImportPlaybackKeyPair $
--             newImportPlaybackKeyPair
--
--         , requestListChannels $
--             newListChannels
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
--         , requestGetStreamKey $
--             newGetStreamKey
--
--         , requestDeletePlaybackKeyPair $
--             newDeletePlaybackKeyPair
--
--         , requestBatchGetStreamKey $
--             newBatchGetStreamKey
--
--         , requestGetPlaybackKeyPair $
--             newGetPlaybackKeyPair
--
--         , requestDeleteRecordingConfiguration $
--             newDeleteRecordingConfiguration
--
--         , requestStopStream $
--             newStopStream
--
--         , requestCreateChannel $
--             newCreateChannel
--
--         , requestDeleteStreamKey $
--             newDeleteStreamKey
--
--         , requestGetStream $
--             newGetStream
--
--         , requestListStreamKeys $
--             newListStreamKeys
--
--         , requestGetChannel $
--             newGetChannel
--
--         , requestListStreams $
--             newListStreams
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetRecordingConfiguration $
--             newGetRecordingConfiguration
--
--         , requestListPlaybackKeyPairs $
--             newListPlaybackKeyPairs
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateRecordingConfiguration $
--             newCreateRecordingConfiguration
--
--         , requestBatchGetChannel $
--             newBatchGetChannel
--
--           ]

--     , testGroup "response"
--         [ responsePutMetadata $
--             newPutMetadataResponse
--
--         , responseListRecordingConfigurations $
--             newListRecordingConfigurationsResponse
--
--         , responseCreateStreamKey $
--             newCreateStreamKeyResponse
--
--         , responseImportPlaybackKeyPair $
--             newImportPlaybackKeyPairResponse
--
--         , responseListChannels $
--             newListChannelsResponse
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
--         , responseGetStreamKey $
--             newGetStreamKeyResponse
--
--         , responseDeletePlaybackKeyPair $
--             newDeletePlaybackKeyPairResponse
--
--         , responseBatchGetStreamKey $
--             newBatchGetStreamKeyResponse
--
--         , responseGetPlaybackKeyPair $
--             newGetPlaybackKeyPairResponse
--
--         , responseDeleteRecordingConfiguration $
--             newDeleteRecordingConfigurationResponse
--
--         , responseStopStream $
--             newStopStreamResponse
--
--         , responseCreateChannel $
--             newCreateChannelResponse
--
--         , responseDeleteStreamKey $
--             newDeleteStreamKeyResponse
--
--         , responseGetStream $
--             newGetStreamResponse
--
--         , responseListStreamKeys $
--             newListStreamKeysResponse
--
--         , responseGetChannel $
--             newGetChannelResponse
--
--         , responseListStreams $
--             newListStreamsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetRecordingConfiguration $
--             newGetRecordingConfigurationResponse
--
--         , responseListPlaybackKeyPairs $
--             newListPlaybackKeyPairsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateRecordingConfiguration $
--             newCreateRecordingConfigurationResponse
--
--         , responseBatchGetChannel $
--             newBatchGetChannelResponse
--
--           ]
--     ]

-- Requests

requestPutMetadata :: PutMetadata -> TestTree
requestPutMetadata =
  req
    "PutMetadata"
    "fixture/PutMetadata.yaml"

requestListRecordingConfigurations :: ListRecordingConfigurations -> TestTree
requestListRecordingConfigurations =
  req
    "ListRecordingConfigurations"
    "fixture/ListRecordingConfigurations.yaml"

requestCreateStreamKey :: CreateStreamKey -> TestTree
requestCreateStreamKey =
  req
    "CreateStreamKey"
    "fixture/CreateStreamKey.yaml"

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

requestGetStreamKey :: GetStreamKey -> TestTree
requestGetStreamKey =
  req
    "GetStreamKey"
    "fixture/GetStreamKey.yaml"

requestDeletePlaybackKeyPair :: DeletePlaybackKeyPair -> TestTree
requestDeletePlaybackKeyPair =
  req
    "DeletePlaybackKeyPair"
    "fixture/DeletePlaybackKeyPair.yaml"

requestBatchGetStreamKey :: BatchGetStreamKey -> TestTree
requestBatchGetStreamKey =
  req
    "BatchGetStreamKey"
    "fixture/BatchGetStreamKey.yaml"

requestGetPlaybackKeyPair :: GetPlaybackKeyPair -> TestTree
requestGetPlaybackKeyPair =
  req
    "GetPlaybackKeyPair"
    "fixture/GetPlaybackKeyPair.yaml"

requestDeleteRecordingConfiguration :: DeleteRecordingConfiguration -> TestTree
requestDeleteRecordingConfiguration =
  req
    "DeleteRecordingConfiguration"
    "fixture/DeleteRecordingConfiguration.yaml"

requestStopStream :: StopStream -> TestTree
requestStopStream =
  req
    "StopStream"
    "fixture/StopStream.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

requestDeleteStreamKey :: DeleteStreamKey -> TestTree
requestDeleteStreamKey =
  req
    "DeleteStreamKey"
    "fixture/DeleteStreamKey.yaml"

requestGetStream :: GetStream -> TestTree
requestGetStream =
  req
    "GetStream"
    "fixture/GetStream.yaml"

requestListStreamKeys :: ListStreamKeys -> TestTree
requestListStreamKeys =
  req
    "ListStreamKeys"
    "fixture/ListStreamKeys.yaml"

requestGetChannel :: GetChannel -> TestTree
requestGetChannel =
  req
    "GetChannel"
    "fixture/GetChannel.yaml"

requestListStreams :: ListStreams -> TestTree
requestListStreams =
  req
    "ListStreams"
    "fixture/ListStreams.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetRecordingConfiguration :: GetRecordingConfiguration -> TestTree
requestGetRecordingConfiguration =
  req
    "GetRecordingConfiguration"
    "fixture/GetRecordingConfiguration.yaml"

requestListPlaybackKeyPairs :: ListPlaybackKeyPairs -> TestTree
requestListPlaybackKeyPairs =
  req
    "ListPlaybackKeyPairs"
    "fixture/ListPlaybackKeyPairs.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateRecordingConfiguration :: CreateRecordingConfiguration -> TestTree
requestCreateRecordingConfiguration =
  req
    "CreateRecordingConfiguration"
    "fixture/CreateRecordingConfiguration.yaml"

requestBatchGetChannel :: BatchGetChannel -> TestTree
requestBatchGetChannel =
  req
    "BatchGetChannel"
    "fixture/BatchGetChannel.yaml"

-- Responses

responsePutMetadata :: PutMetadataResponse -> TestTree
responsePutMetadata =
  res
    "PutMetadataResponse"
    "fixture/PutMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMetadata)

responseListRecordingConfigurations :: ListRecordingConfigurationsResponse -> TestTree
responseListRecordingConfigurations =
  res
    "ListRecordingConfigurationsResponse"
    "fixture/ListRecordingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecordingConfigurations)

responseCreateStreamKey :: CreateStreamKeyResponse -> TestTree
responseCreateStreamKey =
  res
    "CreateStreamKeyResponse"
    "fixture/CreateStreamKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamKey)

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
    (Proxy.Proxy :: Proxy.Proxy UpdateChannel)

responseGetStreamKey :: GetStreamKeyResponse -> TestTree
responseGetStreamKey =
  res
    "GetStreamKeyResponse"
    "fixture/GetStreamKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamKey)

responseDeletePlaybackKeyPair :: DeletePlaybackKeyPairResponse -> TestTree
responseDeletePlaybackKeyPair =
  res
    "DeletePlaybackKeyPairResponse"
    "fixture/DeletePlaybackKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlaybackKeyPair)

responseBatchGetStreamKey :: BatchGetStreamKeyResponse -> TestTree
responseBatchGetStreamKey =
  res
    "BatchGetStreamKeyResponse"
    "fixture/BatchGetStreamKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetStreamKey)

responseGetPlaybackKeyPair :: GetPlaybackKeyPairResponse -> TestTree
responseGetPlaybackKeyPair =
  res
    "GetPlaybackKeyPairResponse"
    "fixture/GetPlaybackKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPlaybackKeyPair)

responseDeleteRecordingConfiguration :: DeleteRecordingConfigurationResponse -> TestTree
responseDeleteRecordingConfiguration =
  res
    "DeleteRecordingConfigurationResponse"
    "fixture/DeleteRecordingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecordingConfiguration)

responseStopStream :: StopStreamResponse -> TestTree
responseStopStream =
  res
    "StopStreamResponse"
    "fixture/StopStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStream)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChannel)

responseDeleteStreamKey :: DeleteStreamKeyResponse -> TestTree
responseDeleteStreamKey =
  res
    "DeleteStreamKeyResponse"
    "fixture/DeleteStreamKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamKey)

responseGetStream :: GetStreamResponse -> TestTree
responseGetStream =
  res
    "GetStreamResponse"
    "fixture/GetStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStream)

responseListStreamKeys :: ListStreamKeysResponse -> TestTree
responseListStreamKeys =
  res
    "ListStreamKeysResponse"
    "fixture/ListStreamKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamKeys)

responseGetChannel :: GetChannelResponse -> TestTree
responseGetChannel =
  res
    "GetChannelResponse"
    "fixture/GetChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChannel)

responseListStreams :: ListStreamsResponse -> TestTree
responseListStreams =
  res
    "ListStreamsResponse"
    "fixture/ListStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreams)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetRecordingConfiguration :: GetRecordingConfigurationResponse -> TestTree
responseGetRecordingConfiguration =
  res
    "GetRecordingConfigurationResponse"
    "fixture/GetRecordingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecordingConfiguration)

responseListPlaybackKeyPairs :: ListPlaybackKeyPairsResponse -> TestTree
responseListPlaybackKeyPairs =
  res
    "ListPlaybackKeyPairsResponse"
    "fixture/ListPlaybackKeyPairsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlaybackKeyPairs)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateRecordingConfiguration :: CreateRecordingConfigurationResponse -> TestTree
responseCreateRecordingConfiguration =
  res
    "CreateRecordingConfigurationResponse"
    "fixture/CreateRecordingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecordingConfiguration)

responseBatchGetChannel :: BatchGetChannelResponse -> TestTree
responseBatchGetChannel =
  res
    "BatchGetChannelResponse"
    "fixture/BatchGetChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetChannel)
