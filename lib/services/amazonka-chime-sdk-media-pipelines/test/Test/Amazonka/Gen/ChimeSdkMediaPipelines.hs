{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ChimeSdkMediaPipelines
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ChimeSdkMediaPipelines where

import Amazonka.ChimeSdkMediaPipelines
import qualified Data.Proxy as Proxy
import Test.Amazonka.ChimeSdkMediaPipelines.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateMediaCapturePipeline $
--             newCreateMediaCapturePipeline
--
--         , requestCreateMediaConcatenationPipeline $
--             newCreateMediaConcatenationPipeline
--
--         , requestCreateMediaLiveConnectorPipeline $
--             newCreateMediaLiveConnectorPipeline
--
--         , requestDeleteMediaCapturePipeline $
--             newDeleteMediaCapturePipeline
--
--         , requestDeleteMediaPipeline $
--             newDeleteMediaPipeline
--
--         , requestGetMediaCapturePipeline $
--             newGetMediaCapturePipeline
--
--         , requestGetMediaPipeline $
--             newGetMediaPipeline
--
--         , requestListMediaCapturePipelines $
--             newListMediaCapturePipelines
--
--         , requestListMediaPipelines $
--             newListMediaPipelines
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCreateMediaCapturePipeline $
--             newCreateMediaCapturePipelineResponse
--
--         , responseCreateMediaConcatenationPipeline $
--             newCreateMediaConcatenationPipelineResponse
--
--         , responseCreateMediaLiveConnectorPipeline $
--             newCreateMediaLiveConnectorPipelineResponse
--
--         , responseDeleteMediaCapturePipeline $
--             newDeleteMediaCapturePipelineResponse
--
--         , responseDeleteMediaPipeline $
--             newDeleteMediaPipelineResponse
--
--         , responseGetMediaCapturePipeline $
--             newGetMediaCapturePipelineResponse
--
--         , responseGetMediaPipeline $
--             newGetMediaPipelineResponse
--
--         , responseListMediaCapturePipelines $
--             newListMediaCapturePipelinesResponse
--
--         , responseListMediaPipelines $
--             newListMediaPipelinesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestCreateMediaCapturePipeline :: CreateMediaCapturePipeline -> TestTree
requestCreateMediaCapturePipeline =
  req
    "CreateMediaCapturePipeline"
    "fixture/CreateMediaCapturePipeline.yaml"

requestCreateMediaConcatenationPipeline :: CreateMediaConcatenationPipeline -> TestTree
requestCreateMediaConcatenationPipeline =
  req
    "CreateMediaConcatenationPipeline"
    "fixture/CreateMediaConcatenationPipeline.yaml"

requestCreateMediaLiveConnectorPipeline :: CreateMediaLiveConnectorPipeline -> TestTree
requestCreateMediaLiveConnectorPipeline =
  req
    "CreateMediaLiveConnectorPipeline"
    "fixture/CreateMediaLiveConnectorPipeline.yaml"

requestDeleteMediaCapturePipeline :: DeleteMediaCapturePipeline -> TestTree
requestDeleteMediaCapturePipeline =
  req
    "DeleteMediaCapturePipeline"
    "fixture/DeleteMediaCapturePipeline.yaml"

requestDeleteMediaPipeline :: DeleteMediaPipeline -> TestTree
requestDeleteMediaPipeline =
  req
    "DeleteMediaPipeline"
    "fixture/DeleteMediaPipeline.yaml"

requestGetMediaCapturePipeline :: GetMediaCapturePipeline -> TestTree
requestGetMediaCapturePipeline =
  req
    "GetMediaCapturePipeline"
    "fixture/GetMediaCapturePipeline.yaml"

requestGetMediaPipeline :: GetMediaPipeline -> TestTree
requestGetMediaPipeline =
  req
    "GetMediaPipeline"
    "fixture/GetMediaPipeline.yaml"

requestListMediaCapturePipelines :: ListMediaCapturePipelines -> TestTree
requestListMediaCapturePipelines =
  req
    "ListMediaCapturePipelines"
    "fixture/ListMediaCapturePipelines.yaml"

requestListMediaPipelines :: ListMediaPipelines -> TestTree
requestListMediaPipelines =
  req
    "ListMediaPipelines"
    "fixture/ListMediaPipelines.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

-- Responses

responseCreateMediaCapturePipeline :: CreateMediaCapturePipelineResponse -> TestTree
responseCreateMediaCapturePipeline =
  res
    "CreateMediaCapturePipelineResponse"
    "fixture/CreateMediaCapturePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMediaCapturePipeline)

responseCreateMediaConcatenationPipeline :: CreateMediaConcatenationPipelineResponse -> TestTree
responseCreateMediaConcatenationPipeline =
  res
    "CreateMediaConcatenationPipelineResponse"
    "fixture/CreateMediaConcatenationPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMediaConcatenationPipeline)

responseCreateMediaLiveConnectorPipeline :: CreateMediaLiveConnectorPipelineResponse -> TestTree
responseCreateMediaLiveConnectorPipeline =
  res
    "CreateMediaLiveConnectorPipelineResponse"
    "fixture/CreateMediaLiveConnectorPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMediaLiveConnectorPipeline)

responseDeleteMediaCapturePipeline :: DeleteMediaCapturePipelineResponse -> TestTree
responseDeleteMediaCapturePipeline =
  res
    "DeleteMediaCapturePipelineResponse"
    "fixture/DeleteMediaCapturePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMediaCapturePipeline)

responseDeleteMediaPipeline :: DeleteMediaPipelineResponse -> TestTree
responseDeleteMediaPipeline =
  res
    "DeleteMediaPipelineResponse"
    "fixture/DeleteMediaPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMediaPipeline)

responseGetMediaCapturePipeline :: GetMediaCapturePipelineResponse -> TestTree
responseGetMediaCapturePipeline =
  res
    "GetMediaCapturePipelineResponse"
    "fixture/GetMediaCapturePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMediaCapturePipeline)

responseGetMediaPipeline :: GetMediaPipelineResponse -> TestTree
responseGetMediaPipeline =
  res
    "GetMediaPipelineResponse"
    "fixture/GetMediaPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMediaPipeline)

responseListMediaCapturePipelines :: ListMediaCapturePipelinesResponse -> TestTree
responseListMediaCapturePipelines =
  res
    "ListMediaCapturePipelinesResponse"
    "fixture/ListMediaCapturePipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMediaCapturePipelines)

responseListMediaPipelines :: ListMediaPipelinesResponse -> TestTree
responseListMediaPipelines =
  res
    "ListMediaPipelinesResponse"
    "fixture/ListMediaPipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMediaPipelines)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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
