{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticTranscoder
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    "fixture/DeletePreset"

testListJobsByPipeline :: ListJobsByPipeline -> TestTree
testListJobsByPipeline = req
    "ListJobsByPipeline"
    "fixture/ListJobsByPipeline"

testUpdatePipelineStatus :: UpdatePipelineStatus -> TestTree
testUpdatePipelineStatus = req
    "UpdatePipelineStatus"
    "fixture/UpdatePipelineStatus"

testTestRole :: TestRole -> TestTree
testTestRole = req
    "TestRole"
    "fixture/TestRole"

testUpdatePipeline :: UpdatePipeline -> TestTree
testUpdatePipeline = req
    "UpdatePipeline"
    "fixture/UpdatePipeline"

testDeletePipeline :: DeletePipeline -> TestTree
testDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline"

testCreateJob :: CreateJob -> TestTree
testCreateJob = req
    "CreateJob"
    "fixture/CreateJob"

testListPipelines :: ListPipelines -> TestTree
testListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines"

testCreatePreset :: CreatePreset -> TestTree
testCreatePreset = req
    "CreatePreset"
    "fixture/CreatePreset"

testListPresets :: ListPresets -> TestTree
testListPresets = req
    "ListPresets"
    "fixture/ListPresets"

testReadPreset :: ReadPreset -> TestTree
testReadPreset = req
    "ReadPreset"
    "fixture/ReadPreset"

testUpdatePipelineNotifications :: UpdatePipelineNotifications -> TestTree
testUpdatePipelineNotifications = req
    "UpdatePipelineNotifications"
    "fixture/UpdatePipelineNotifications"

testReadJob :: ReadJob -> TestTree
testReadJob = req
    "ReadJob"
    "fixture/ReadJob"

testReadPipeline :: ReadPipeline -> TestTree
testReadPipeline = req
    "ReadPipeline"
    "fixture/ReadPipeline"

testCreatePipeline :: CreatePipeline -> TestTree
testCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline"

testListJobsByStatus :: ListJobsByStatus -> TestTree
testListJobsByStatus = req
    "ListJobsByStatus"
    "fixture/ListJobsByStatus"

testCancelJob :: CancelJob -> TestTree
testCancelJob = req
    "CancelJob"
    "fixture/CancelJob"

-- Responses

testDeletePresetResponse :: DeletePresetResponse -> TestTree
testDeletePresetResponse = res
    "DeletePresetResponse"
    "fixture/DeletePresetResponse"
    (Proxy :: Proxy DeletePreset)

testListJobsByPipelineResponse :: ListJobsByPipelineResponse -> TestTree
testListJobsByPipelineResponse = res
    "ListJobsByPipelineResponse"
    "fixture/ListJobsByPipelineResponse"
    (Proxy :: Proxy ListJobsByPipeline)

testUpdatePipelineStatusResponse :: UpdatePipelineStatusResponse -> TestTree
testUpdatePipelineStatusResponse = res
    "UpdatePipelineStatusResponse"
    "fixture/UpdatePipelineStatusResponse"
    (Proxy :: Proxy UpdatePipelineStatus)

testTestRoleResponse :: TestRoleResponse -> TestTree
testTestRoleResponse = res
    "TestRoleResponse"
    "fixture/TestRoleResponse"
    (Proxy :: Proxy TestRole)

testUpdatePipelineResponse :: UpdatePipelineResponse -> TestTree
testUpdatePipelineResponse = res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse"
    (Proxy :: Proxy UpdatePipeline)

testDeletePipelineResponse :: DeletePipelineResponse -> TestTree
testDeletePipelineResponse = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse"
    (Proxy :: Proxy DeletePipeline)

testCreateJobResponse :: CreateJobResponse -> TestTree
testCreateJobResponse = res
    "CreateJobResponse"
    "fixture/CreateJobResponse"
    (Proxy :: Proxy CreateJob)

testListPipelinesResponse :: ListPipelinesResponse -> TestTree
testListPipelinesResponse = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse"
    (Proxy :: Proxy ListPipelines)

testCreatePresetResponse :: CreatePresetResponse -> TestTree
testCreatePresetResponse = res
    "CreatePresetResponse"
    "fixture/CreatePresetResponse"
    (Proxy :: Proxy CreatePreset)

testListPresetsResponse :: ListPresetsResponse -> TestTree
testListPresetsResponse = res
    "ListPresetsResponse"
    "fixture/ListPresetsResponse"
    (Proxy :: Proxy ListPresets)

testReadPresetResponse :: ReadPresetResponse -> TestTree
testReadPresetResponse = res
    "ReadPresetResponse"
    "fixture/ReadPresetResponse"
    (Proxy :: Proxy ReadPreset)

testUpdatePipelineNotificationsResponse :: UpdatePipelineNotificationsResponse -> TestTree
testUpdatePipelineNotificationsResponse = res
    "UpdatePipelineNotificationsResponse"
    "fixture/UpdatePipelineNotificationsResponse"
    (Proxy :: Proxy UpdatePipelineNotifications)

testReadJobResponse :: ReadJobResponse -> TestTree
testReadJobResponse = res
    "ReadJobResponse"
    "fixture/ReadJobResponse"
    (Proxy :: Proxy ReadJob)

testReadPipelineResponse :: ReadPipelineResponse -> TestTree
testReadPipelineResponse = res
    "ReadPipelineResponse"
    "fixture/ReadPipelineResponse"
    (Proxy :: Proxy ReadPipeline)

testCreatePipelineResponse :: CreatePipelineResponse -> TestTree
testCreatePipelineResponse = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse"
    (Proxy :: Proxy CreatePipeline)

testListJobsByStatusResponse :: ListJobsByStatusResponse -> TestTree
testListJobsByStatusResponse = res
    "ListJobsByStatusResponse"
    "fixture/ListJobsByStatusResponse"
    (Proxy :: Proxy ListJobsByStatus)

testCancelJobResponse :: CancelJobResponse -> TestTree
testCancelJobResponse = res
    "CancelJobResponse"
    "fixture/CancelJobResponse"
    (Proxy :: Proxy CancelJob)

instance Out Artwork
instance Out AudioCodecOptions
instance Out AudioParameters
instance Out CancelJob
instance Out CancelJobResponse
instance Out CaptionFormat
instance Out CaptionSource
instance Out Captions
instance Out Clip
instance Out CreateJob
instance Out CreateJobOutput
instance Out CreateJobPlaylist
instance Out CreateJobResponse
instance Out CreatePipeline
instance Out CreatePipelineResponse
instance Out CreatePreset
instance Out CreatePresetResponse
instance Out DeletePipeline
instance Out DeletePipelineResponse
instance Out DeletePreset
instance Out DeletePresetResponse
instance Out DetectedProperties
instance Out Encryption
instance Out HlsContentProtection
instance Out Job'
instance Out JobAlbumArt
instance Out JobInput
instance Out JobOutput
instance Out JobWatermark
instance Out ListJobsByPipeline
instance Out ListJobsByPipelineResponse
instance Out ListJobsByStatus
instance Out ListJobsByStatusResponse
instance Out ListPipelines
instance Out ListPipelinesResponse
instance Out ListPresets
instance Out ListPresetsResponse
instance Out Notifications
instance Out Permission
instance Out Pipeline
instance Out PipelineOutputConfig
instance Out PlayReadyDrm
instance Out Playlist
instance Out Preset
instance Out PresetWatermark
instance Out ReadJob
instance Out ReadJobResponse
instance Out ReadPipeline
instance Out ReadPipelineResponse
instance Out ReadPreset
instance Out ReadPresetResponse
instance Out TestRole
instance Out TestRoleResponse
instance Out Thumbnails
instance Out TimeSpan
instance Out Timing
instance Out UpdatePipeline
instance Out UpdatePipelineNotifications
instance Out UpdatePipelineNotificationsResponse
instance Out UpdatePipelineResponse
instance Out UpdatePipelineStatus
instance Out UpdatePipelineStatusResponse
instance Out VideoParameters
instance Out Warning
