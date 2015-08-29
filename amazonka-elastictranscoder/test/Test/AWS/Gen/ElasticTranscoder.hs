{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticTranscoder
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ElasticTranscoder where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ElasticTranscoder
import Test.AWS.ElasticTranscoder.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDeletePreset $
--             deletePreset
--
--         , testListJobsByPipeline $
--             listJobsByPipeline
--
--         , testUpdatePipelineStatus $
--             updatePipelineStatus
--
--         , testTestRole $
--             testRole
--
--         , testUpdatePipeline $
--             updatePipeline
--
--         , testDeletePipeline $
--             deletePipeline
--
--         , testCreateJob $
--             createJob
--
--         , testListPipelines $
--             listPipelines
--
--         , testCreatePreset $
--             createPreset
--
--         , testListPresets $
--             listPresets
--
--         , testReadPreset $
--             readPreset
--
--         , testUpdatePipelineNotifications $
--             updatePipelineNotifications
--
--         , testReadJob $
--             readJob
--
--         , testReadPipeline $
--             readPipeline
--
--         , testCreatePipeline $
--             createPipeline
--
--         , testListJobsByStatus $
--             listJobsByStatus
--
--         , testCancelJob $
--             cancelJob
--
--           ]

--     , testGroup "response"
--         [ testDeletePresetResponse $
--             deletePresetResponse
--
--         , testListJobsByPipelineResponse $
--             listJobsByPipelineResponse
--
--         , testUpdatePipelineStatusResponse $
--             updatePipelineStatusResponse
--
--         , testTestRoleResponse $
--             testRoleResponse
--
--         , testUpdatePipelineResponse $
--             updatePipelineResponse
--
--         , testDeletePipelineResponse $
--             deletePipelineResponse
--
--         , testCreateJobResponse $
--             createJobResponse
--
--         , testListPipelinesResponse $
--             listPipelinesResponse
--
--         , testCreatePresetResponse $
--             createPresetResponse
--
--         , testListPresetsResponse $
--             listPresetsResponse
--
--         , testReadPresetResponse $
--             readPresetResponse
--
--         , testUpdatePipelineNotificationsResponse $
--             updatePipelineNotificationsResponse
--
--         , testReadJobResponse $
--             readJobResponse
--
--         , testReadPipelineResponse $
--             readPipelineResponse
--
--         , testCreatePipelineResponse $
--             createPipelineResponse
--
--         , testListJobsByStatusResponse $
--             listJobsByStatusResponse
--
--         , testCancelJobResponse $
--             cancelJobResponse
--
--           ]
--     ]

-- Requests

testDeletePreset :: DeletePreset -> TestTree
testDeletePreset = req
    "DeletePreset"
    "fixture/DeletePreset.yaml"

testListJobsByPipeline :: ListJobsByPipeline -> TestTree
testListJobsByPipeline = req
    "ListJobsByPipeline"
    "fixture/ListJobsByPipeline.yaml"

testUpdatePipelineStatus :: UpdatePipelineStatus -> TestTree
testUpdatePipelineStatus = req
    "UpdatePipelineStatus"
    "fixture/UpdatePipelineStatus.yaml"

testTestRole :: TestRole -> TestTree
testTestRole = req
    "TestRole"
    "fixture/TestRole.yaml"

testUpdatePipeline :: UpdatePipeline -> TestTree
testUpdatePipeline = req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

testDeletePipeline :: DeletePipeline -> TestTree
testDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

testCreateJob :: CreateJob -> TestTree
testCreateJob = req
    "CreateJob"
    "fixture/CreateJob.yaml"

testListPipelines :: ListPipelines -> TestTree
testListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

testCreatePreset :: CreatePreset -> TestTree
testCreatePreset = req
    "CreatePreset"
    "fixture/CreatePreset.yaml"

testListPresets :: ListPresets -> TestTree
testListPresets = req
    "ListPresets"
    "fixture/ListPresets.yaml"

testReadPreset :: ReadPreset -> TestTree
testReadPreset = req
    "ReadPreset"
    "fixture/ReadPreset.yaml"

testUpdatePipelineNotifications :: UpdatePipelineNotifications -> TestTree
testUpdatePipelineNotifications = req
    "UpdatePipelineNotifications"
    "fixture/UpdatePipelineNotifications.yaml"

testReadJob :: ReadJob -> TestTree
testReadJob = req
    "ReadJob"
    "fixture/ReadJob.yaml"

testReadPipeline :: ReadPipeline -> TestTree
testReadPipeline = req
    "ReadPipeline"
    "fixture/ReadPipeline.yaml"

testCreatePipeline :: CreatePipeline -> TestTree
testCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

testListJobsByStatus :: ListJobsByStatus -> TestTree
testListJobsByStatus = req
    "ListJobsByStatus"
    "fixture/ListJobsByStatus.yaml"

testCancelJob :: CancelJob -> TestTree
testCancelJob = req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

testDeletePresetResponse :: DeletePresetResponse -> TestTree
testDeletePresetResponse = res
    "DeletePresetResponse"
    "fixture/DeletePresetResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy DeletePreset)

testListJobsByPipelineResponse :: ListJobsByPipelineResponse -> TestTree
testListJobsByPipelineResponse = res
    "ListJobsByPipelineResponse"
    "fixture/ListJobsByPipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ListJobsByPipeline)

testUpdatePipelineStatusResponse :: UpdatePipelineStatusResponse -> TestTree
testUpdatePipelineStatusResponse = res
    "UpdatePipelineStatusResponse"
    "fixture/UpdatePipelineStatusResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy UpdatePipelineStatus)

testTestRoleResponse :: TestRoleResponse -> TestTree
testTestRoleResponse = res
    "TestRoleResponse"
    "fixture/TestRoleResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy TestRole)

testUpdatePipelineResponse :: UpdatePipelineResponse -> TestTree
testUpdatePipelineResponse = res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy UpdatePipeline)

testDeletePipelineResponse :: DeletePipelineResponse -> TestTree
testDeletePipelineResponse = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy DeletePipeline)

testCreateJobResponse :: CreateJobResponse -> TestTree
testCreateJobResponse = res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy CreateJob)

testListPipelinesResponse :: ListPipelinesResponse -> TestTree
testListPipelinesResponse = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ListPipelines)

testCreatePresetResponse :: CreatePresetResponse -> TestTree
testCreatePresetResponse = res
    "CreatePresetResponse"
    "fixture/CreatePresetResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy CreatePreset)

testListPresetsResponse :: ListPresetsResponse -> TestTree
testListPresetsResponse = res
    "ListPresetsResponse"
    "fixture/ListPresetsResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ListPresets)

testReadPresetResponse :: ReadPresetResponse -> TestTree
testReadPresetResponse = res
    "ReadPresetResponse"
    "fixture/ReadPresetResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ReadPreset)

testUpdatePipelineNotificationsResponse :: UpdatePipelineNotificationsResponse -> TestTree
testUpdatePipelineNotificationsResponse = res
    "UpdatePipelineNotificationsResponse"
    "fixture/UpdatePipelineNotificationsResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy UpdatePipelineNotifications)

testReadJobResponse :: ReadJobResponse -> TestTree
testReadJobResponse = res
    "ReadJobResponse"
    "fixture/ReadJobResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ReadJob)

testReadPipelineResponse :: ReadPipelineResponse -> TestTree
testReadPipelineResponse = res
    "ReadPipelineResponse"
    "fixture/ReadPipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ReadPipeline)

testCreatePipelineResponse :: CreatePipelineResponse -> TestTree
testCreatePipelineResponse = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy CreatePipeline)

testListJobsByStatusResponse :: ListJobsByStatusResponse -> TestTree
testListJobsByStatusResponse = res
    "ListJobsByStatusResponse"
    "fixture/ListJobsByStatusResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy ListJobsByStatus)

testCancelJobResponse :: CancelJobResponse -> TestTree
testCancelJobResponse = res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    elasticTranscoder
    (Proxy :: Proxy CancelJob)
