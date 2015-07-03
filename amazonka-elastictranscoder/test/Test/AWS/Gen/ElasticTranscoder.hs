-- Module      : Test.AWS.Gen.ElasticTranscoder
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.ElasticTranscoder where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.ElasticTranscoder

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
testDeletePreset = undefined

testListJobsByPipeline :: ListJobsByPipeline -> TestTree
testListJobsByPipeline = undefined

testUpdatePipelineStatus :: UpdatePipelineStatus -> TestTree
testUpdatePipelineStatus = undefined

testTestRole :: TestRole -> TestTree
testTestRole = undefined

testUpdatePipeline :: UpdatePipeline -> TestTree
testUpdatePipeline = undefined

testDeletePipeline :: DeletePipeline -> TestTree
testDeletePipeline = undefined

testCreateJob :: CreateJob -> TestTree
testCreateJob = undefined

testListPipelines :: ListPipelines -> TestTree
testListPipelines = undefined

testCreatePreset :: CreatePreset -> TestTree
testCreatePreset = undefined

testListPresets :: ListPresets -> TestTree
testListPresets = undefined

testReadPreset :: ReadPreset -> TestTree
testReadPreset = undefined

testUpdatePipelineNotifications :: UpdatePipelineNotifications -> TestTree
testUpdatePipelineNotifications = undefined

testReadJob :: ReadJob -> TestTree
testReadJob = undefined

testReadPipeline :: ReadPipeline -> TestTree
testReadPipeline = undefined

testCreatePipeline :: CreatePipeline -> TestTree
testCreatePipeline = undefined

testListJobsByStatus :: ListJobsByStatus -> TestTree
testListJobsByStatus = undefined

testCancelJob :: CancelJob -> TestTree
testCancelJob = undefined

-- Responses

testDeletePresetResponse :: DeletePresetResponse -> TestTree
testDeletePresetResponse = resp
    "DeletePresetResponse"
    "fixture/DeletePresetResponse"
    (Proxy :: Proxy DeletePreset)

testListJobsByPipelineResponse :: ListJobsByPipelineResponse -> TestTree
testListJobsByPipelineResponse = resp
    "ListJobsByPipelineResponse"
    "fixture/ListJobsByPipelineResponse"
    (Proxy :: Proxy ListJobsByPipeline)

testUpdatePipelineStatusResponse :: UpdatePipelineStatusResponse -> TestTree
testUpdatePipelineStatusResponse = resp
    "UpdatePipelineStatusResponse"
    "fixture/UpdatePipelineStatusResponse"
    (Proxy :: Proxy UpdatePipelineStatus)

testTestRoleResponse :: TestRoleResponse -> TestTree
testTestRoleResponse = resp
    "TestRoleResponse"
    "fixture/TestRoleResponse"
    (Proxy :: Proxy TestRole)

testUpdatePipelineResponse :: UpdatePipelineResponse -> TestTree
testUpdatePipelineResponse = resp
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse"
    (Proxy :: Proxy UpdatePipeline)

testDeletePipelineResponse :: DeletePipelineResponse -> TestTree
testDeletePipelineResponse = resp
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse"
    (Proxy :: Proxy DeletePipeline)

testCreateJobResponse :: CreateJobResponse -> TestTree
testCreateJobResponse = resp
    "CreateJobResponse"
    "fixture/CreateJobResponse"
    (Proxy :: Proxy CreateJob)

testListPipelinesResponse :: ListPipelinesResponse -> TestTree
testListPipelinesResponse = resp
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse"
    (Proxy :: Proxy ListPipelines)

testCreatePresetResponse :: CreatePresetResponse -> TestTree
testCreatePresetResponse = resp
    "CreatePresetResponse"
    "fixture/CreatePresetResponse"
    (Proxy :: Proxy CreatePreset)

testListPresetsResponse :: ListPresetsResponse -> TestTree
testListPresetsResponse = resp
    "ListPresetsResponse"
    "fixture/ListPresetsResponse"
    (Proxy :: Proxy ListPresets)

testReadPresetResponse :: ReadPresetResponse -> TestTree
testReadPresetResponse = resp
    "ReadPresetResponse"
    "fixture/ReadPresetResponse"
    (Proxy :: Proxy ReadPreset)

testUpdatePipelineNotificationsResponse :: UpdatePipelineNotificationsResponse -> TestTree
testUpdatePipelineNotificationsResponse = resp
    "UpdatePipelineNotificationsResponse"
    "fixture/UpdatePipelineNotificationsResponse"
    (Proxy :: Proxy UpdatePipelineNotifications)

testReadJobResponse :: ReadJobResponse -> TestTree
testReadJobResponse = resp
    "ReadJobResponse"
    "fixture/ReadJobResponse"
    (Proxy :: Proxy ReadJob)

testReadPipelineResponse :: ReadPipelineResponse -> TestTree
testReadPipelineResponse = resp
    "ReadPipelineResponse"
    "fixture/ReadPipelineResponse"
    (Proxy :: Proxy ReadPipeline)

testCreatePipelineResponse :: CreatePipelineResponse -> TestTree
testCreatePipelineResponse = resp
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse"
    (Proxy :: Proxy CreatePipeline)

testListJobsByStatusResponse :: ListJobsByStatusResponse -> TestTree
testListJobsByStatusResponse = resp
    "ListJobsByStatusResponse"
    "fixture/ListJobsByStatusResponse"
    (Proxy :: Proxy ListJobsByStatus)

testCancelJobResponse :: CancelJobResponse -> TestTree
testCancelJobResponse = resp
    "CancelJobResponse"
    "fixture/CancelJobResponse"
    (Proxy :: Proxy CancelJob)
