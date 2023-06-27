{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.OsIs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.OsIs where

import Amazonka.OsIs
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.OsIs.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreatePipeline $
--             newCreatePipeline
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestGetPipeline $
--             newGetPipeline
--
--         , requestGetPipelineBlueprint $
--             newGetPipelineBlueprint
--
--         , requestGetPipelineChangeProgress $
--             newGetPipelineChangeProgress
--
--         , requestListPipelineBlueprints $
--             newListPipelineBlueprints
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartPipeline $
--             newStartPipeline
--
--         , requestStopPipeline $
--             newStopPipeline
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestValidatePipeline $
--             newValidatePipeline
--
--           ]

--     , testGroup "response"
--         [ responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseGetPipeline $
--             newGetPipelineResponse
--
--         , responseGetPipelineBlueprint $
--             newGetPipelineBlueprintResponse
--
--         , responseGetPipelineChangeProgress $
--             newGetPipelineChangeProgressResponse
--
--         , responseListPipelineBlueprints $
--             newListPipelineBlueprintsResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartPipeline $
--             newStartPipelineResponse
--
--         , responseStopPipeline $
--             newStopPipelineResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseValidatePipeline $
--             newValidatePipelineResponse
--
--           ]
--     ]

-- Requests

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestGetPipeline :: GetPipeline -> TestTree
requestGetPipeline =
  req
    "GetPipeline"
    "fixture/GetPipeline.yaml"

requestGetPipelineBlueprint :: GetPipelineBlueprint -> TestTree
requestGetPipelineBlueprint =
  req
    "GetPipelineBlueprint"
    "fixture/GetPipelineBlueprint.yaml"

requestGetPipelineChangeProgress :: GetPipelineChangeProgress -> TestTree
requestGetPipelineChangeProgress =
  req
    "GetPipelineChangeProgress"
    "fixture/GetPipelineChangeProgress.yaml"

requestListPipelineBlueprints :: ListPipelineBlueprints -> TestTree
requestListPipelineBlueprints =
  req
    "ListPipelineBlueprints"
    "fixture/ListPipelineBlueprints.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartPipeline :: StartPipeline -> TestTree
requestStartPipeline =
  req
    "StartPipeline"
    "fixture/StartPipeline.yaml"

requestStopPipeline :: StopPipeline -> TestTree
requestStopPipeline =
  req
    "StopPipeline"
    "fixture/StopPipeline.yaml"

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

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestValidatePipeline :: ValidatePipeline -> TestTree
requestValidatePipeline =
  req
    "ValidatePipeline"
    "fixture/ValidatePipeline.yaml"

-- Responses

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePipeline)

responseGetPipeline :: GetPipelineResponse -> TestTree
responseGetPipeline =
  res
    "GetPipelineResponse"
    "fixture/GetPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipeline)

responseGetPipelineBlueprint :: GetPipelineBlueprintResponse -> TestTree
responseGetPipelineBlueprint =
  res
    "GetPipelineBlueprintResponse"
    "fixture/GetPipelineBlueprintResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipelineBlueprint)

responseGetPipelineChangeProgress :: GetPipelineChangeProgressResponse -> TestTree
responseGetPipelineChangeProgress =
  res
    "GetPipelineChangeProgressResponse"
    "fixture/GetPipelineChangeProgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipelineChangeProgress)

responseListPipelineBlueprints :: ListPipelineBlueprintsResponse -> TestTree
responseListPipelineBlueprints =
  res
    "ListPipelineBlueprintsResponse"
    "fixture/ListPipelineBlueprintsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelineBlueprints)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelines)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartPipeline :: StartPipelineResponse -> TestTree
responseStartPipeline =
  res
    "StartPipelineResponse"
    "fixture/StartPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPipeline)

responseStopPipeline :: StopPipelineResponse -> TestTree
responseStopPipeline =
  res
    "StopPipelineResponse"
    "fixture/StopPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopPipeline)

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

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipeline)

responseValidatePipeline :: ValidatePipelineResponse -> TestTree
responseValidatePipeline =
  res
    "ValidatePipelineResponse"
    "fixture/ValidatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidatePipeline)
