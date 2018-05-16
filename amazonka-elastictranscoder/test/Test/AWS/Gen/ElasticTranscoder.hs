{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticTranscoder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ElasticTranscoder where

import Data.Proxy
import Network.AWS.ElasticTranscoder
import Test.AWS.ElasticTranscoder.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeletePreset $
--             deletePreset
--
--         , requestUpdatePipelineStatus $
--             updatePipelineStatus
--
--         , requestListJobsByPipeline $
--             listJobsByPipeline
--
--         , requestUpdatePipeline $
--             updatePipeline
--
--         , requestDeletePipeline $
--             deletePipeline
--
--         , requestCreateJob $
--             createJob
--
--         , requestListPipelines $
--             listPipelines
--
--         , requestCreatePreset $
--             createPreset
--
--         , requestListPresets $
--             listPresets
--
--         , requestReadPreset $
--             readPreset
--
--         , requestReadJob $
--             readJob
--
--         , requestUpdatePipelineNotifications $
--             updatePipelineNotifications
--
--         , requestReadPipeline $
--             readPipeline
--
--         , requestCreatePipeline $
--             createPipeline
--
--         , requestListJobsByStatus $
--             listJobsByStatus
--
--         , requestCancelJob $
--             cancelJob
--
--           ]

--     , testGroup "response"
--         [ responseDeletePreset $
--             deletePresetResponse
--
--         , responseUpdatePipelineStatus $
--             updatePipelineStatusResponse
--
--         , responseListJobsByPipeline $
--             listJobsByPipelineResponse
--
--         , responseUpdatePipeline $
--             updatePipelineResponse
--
--         , responseDeletePipeline $
--             deletePipelineResponse
--
--         , responseCreateJob $
--             createJobResponse
--
--         , responseListPipelines $
--             listPipelinesResponse
--
--         , responseCreatePreset $
--             createPresetResponse
--
--         , responseListPresets $
--             listPresetsResponse
--
--         , responseReadPreset $
--             readPresetResponse
--
--         , responseReadJob $
--             readJobResponse
--
--         , responseUpdatePipelineNotifications $
--             updatePipelineNotificationsResponse
--
--         , responseReadPipeline $
--             readPipelineResponse
--
--         , responseCreatePipeline $
--             createPipelineResponse
--
--         , responseListJobsByStatus $
--             listJobsByStatusResponse
--
--         , responseCancelJob $
--             cancelJobResponse
--
--           ]
--     ]

-- Requests

requestDeletePreset :: DeletePreset -> TestTree
requestDeletePreset = req
    "DeletePreset"
    "fixture/DeletePreset.yaml"

requestUpdatePipelineStatus :: UpdatePipelineStatus -> TestTree
requestUpdatePipelineStatus = req
    "UpdatePipelineStatus"
    "fixture/UpdatePipelineStatus.yaml"

requestListJobsByPipeline :: ListJobsByPipeline -> TestTree
requestListJobsByPipeline = req
    "ListJobsByPipeline"
    "fixture/ListJobsByPipeline.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline = req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob = req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestCreatePreset :: CreatePreset -> TestTree
requestCreatePreset = req
    "CreatePreset"
    "fixture/CreatePreset.yaml"

requestListPresets :: ListPresets -> TestTree
requestListPresets = req
    "ListPresets"
    "fixture/ListPresets.yaml"

requestReadPreset :: ReadPreset -> TestTree
requestReadPreset = req
    "ReadPreset"
    "fixture/ReadPreset.yaml"

requestReadJob :: ReadJob -> TestTree
requestReadJob = req
    "ReadJob"
    "fixture/ReadJob.yaml"

requestUpdatePipelineNotifications :: UpdatePipelineNotifications -> TestTree
requestUpdatePipelineNotifications = req
    "UpdatePipelineNotifications"
    "fixture/UpdatePipelineNotifications.yaml"

requestReadPipeline :: ReadPipeline -> TestTree
requestReadPipeline = req
    "ReadPipeline"
    "fixture/ReadPipeline.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestListJobsByStatus :: ListJobsByStatus -> TestTree
requestListJobsByStatus = req
    "ListJobsByStatus"
    "fixture/ListJobsByStatus.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob = req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

responseDeletePreset :: DeletePresetResponse -> TestTree
responseDeletePreset = res
    "DeletePresetResponse"
    "fixture/DeletePresetResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy DeletePreset)

responseUpdatePipelineStatus :: UpdatePipelineStatusResponse -> TestTree
responseUpdatePipelineStatus = res
    "UpdatePipelineStatusResponse"
    "fixture/UpdatePipelineStatusResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy UpdatePipelineStatus)

responseListJobsByPipeline :: ListJobsByPipelineResponse -> TestTree
responseListJobsByPipeline = res
    "ListJobsByPipelineResponse"
    "fixture/ListJobsByPipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ListJobsByPipeline)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline = res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy UpdatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy DeletePipeline)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob = res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy CreateJob)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ListPipelines)

responseCreatePreset :: CreatePresetResponse -> TestTree
responseCreatePreset = res
    "CreatePresetResponse"
    "fixture/CreatePresetResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy CreatePreset)

responseListPresets :: ListPresetsResponse -> TestTree
responseListPresets = res
    "ListPresetsResponse"
    "fixture/ListPresetsResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ListPresets)

responseReadPreset :: ReadPresetResponse -> TestTree
responseReadPreset = res
    "ReadPresetResponse"
    "fixture/ReadPresetResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ReadPreset)

responseReadJob :: ReadJobResponse -> TestTree
responseReadJob = res
    "ReadJobResponse"
    "fixture/ReadJobResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ReadJob)

responseUpdatePipelineNotifications :: UpdatePipelineNotificationsResponse -> TestTree
responseUpdatePipelineNotifications = res
    "UpdatePipelineNotificationsResponse"
    "fixture/UpdatePipelineNotificationsResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy UpdatePipelineNotifications)

responseReadPipeline :: ReadPipelineResponse -> TestTree
responseReadPipeline = res
    "ReadPipelineResponse"
    "fixture/ReadPipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ReadPipeline)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy CreatePipeline)

responseListJobsByStatus :: ListJobsByStatusResponse -> TestTree
responseListJobsByStatus = res
    "ListJobsByStatusResponse"
    "fixture/ListJobsByStatusResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ListJobsByStatus)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob = res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy CancelJob)
