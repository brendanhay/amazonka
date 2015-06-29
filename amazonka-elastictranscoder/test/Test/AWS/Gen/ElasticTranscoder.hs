-- Module      : Test.AWS.Gen.ElasticTranscoder
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ deletePresetTest $
--             deletePreset
--
--         , listJobsByPipelineTest $
--             listJobsByPipeline
--
--         , updatePipelineStatusTest $
--             updatePipelineStatus
--
--         , testRoleTest $
--             testRole
--
--         , updatePipelineTest $
--             updatePipeline
--
--         , deletePipelineTest $
--             deletePipeline
--
--         , createJobTest $
--             createJob
--
--         , listPipelinesTest $
--             listPipelines
--
--         , createPresetTest $
--             createPreset
--
--         , listPresetsTest $
--             listPresets
--
--         , readPresetTest $
--             readPreset
--
--         , updatePipelineNotificationsTest $
--             updatePipelineNotifications
--
--         , readJobTest $
--             readJob
--
--         , readPipelineTest $
--             readPipeline
--
--         , createPipelineTest $
--             createPipeline
--
--         , listJobsByStatusTest $
--             listJobsByStatus
--
--         , cancelJobTest $
--             cancelJob
--
--           ]

--     , testGroup "response"
--         [ deletePresetResponseTest $
--             deletePresetResponse
--
--         , listJobsByPipelineResponseTest $
--             listJobsByPipelineResponse
--
--         , updatePipelineStatusResponseTest $
--             updatePipelineStatusResponse
--
--         , testRoleResponseTest $
--             testRoleResponse
--
--         , updatePipelineResponseTest $
--             updatePipelineResponse
--
--         , deletePipelineResponseTest $
--             deletePipelineResponse
--
--         , createJobResponseTest $
--             createJobResponse
--
--         , listPipelinesResponseTest $
--             listPipelinesResponse
--
--         , createPresetResponseTest $
--             createPresetResponse
--
--         , listPresetsResponseTest $
--             listPresetsResponse
--
--         , readPresetResponseTest $
--             readPresetResponse
--
--         , updatePipelineNotificationsResponseTest $
--             updatePipelineNotificationsResponse
--
--         , readJobResponseTest $
--             readJobResponse
--
--         , readPipelineResponseTest $
--             readPipelineResponse
--
--         , createPipelineResponseTest $
--             createPipelineResponse
--
--         , listJobsByStatusResponseTest $
--             listJobsByStatusResponse
--
--         , cancelJobResponseTest $
--             cancelJobResponse
--
--           ]
--     ]

-- Requests

deletePresetTest :: DeletePreset -> TestTree
deletePresetTest = undefined

listJobsByPipelineTest :: ListJobsByPipeline -> TestTree
listJobsByPipelineTest = undefined

updatePipelineStatusTest :: UpdatePipelineStatus -> TestTree
updatePipelineStatusTest = undefined

testRoleTest :: TestRole -> TestTree
testRoleTest = undefined

updatePipelineTest :: UpdatePipeline -> TestTree
updatePipelineTest = undefined

deletePipelineTest :: DeletePipeline -> TestTree
deletePipelineTest = undefined

createJobTest :: CreateJob -> TestTree
createJobTest = undefined

listPipelinesTest :: ListPipelines -> TestTree
listPipelinesTest = undefined

createPresetTest :: CreatePreset -> TestTree
createPresetTest = undefined

listPresetsTest :: ListPresets -> TestTree
listPresetsTest = undefined

readPresetTest :: ReadPreset -> TestTree
readPresetTest = undefined

updatePipelineNotificationsTest :: UpdatePipelineNotifications -> TestTree
updatePipelineNotificationsTest = undefined

readJobTest :: ReadJob -> TestTree
readJobTest = undefined

readPipelineTest :: ReadPipeline -> TestTree
readPipelineTest = undefined

createPipelineTest :: CreatePipeline -> TestTree
createPipelineTest = undefined

listJobsByStatusTest :: ListJobsByStatus -> TestTree
listJobsByStatusTest = undefined

cancelJobTest :: CancelJob -> TestTree
cancelJobTest = undefined

-- Responses

deletePresetResponseTest :: DeletePresetResponse -> TestTree
deletePresetResponseTest = resp
    "DeletePresetResponse"
    "fixture/DeletePresetResponse"
    (Proxy :: Proxy DeletePreset)

listJobsByPipelineResponseTest :: ListJobsByPipelineResponse -> TestTree
listJobsByPipelineResponseTest = resp
    "ListJobsByPipelineResponse"
    "fixture/ListJobsByPipelineResponse"
    (Proxy :: Proxy ListJobsByPipeline)

updatePipelineStatusResponseTest :: UpdatePipelineStatusResponse -> TestTree
updatePipelineStatusResponseTest = resp
    "UpdatePipelineStatusResponse"
    "fixture/UpdatePipelineStatusResponse"
    (Proxy :: Proxy UpdatePipelineStatus)

testRoleResponseTest :: TestRoleResponse -> TestTree
testRoleResponseTest = resp
    "TestRoleResponse"
    "fixture/TestRoleResponse"
    (Proxy :: Proxy TestRole)

updatePipelineResponseTest :: UpdatePipelineResponse -> TestTree
updatePipelineResponseTest = resp
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse"
    (Proxy :: Proxy UpdatePipeline)

deletePipelineResponseTest :: DeletePipelineResponse -> TestTree
deletePipelineResponseTest = resp
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse"
    (Proxy :: Proxy DeletePipeline)

createJobResponseTest :: CreateJobResponse -> TestTree
createJobResponseTest = resp
    "CreateJobResponse"
    "fixture/CreateJobResponse"
    (Proxy :: Proxy CreateJob)

listPipelinesResponseTest :: ListPipelinesResponse -> TestTree
listPipelinesResponseTest = resp
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse"
    (Proxy :: Proxy ListPipelines)

createPresetResponseTest :: CreatePresetResponse -> TestTree
createPresetResponseTest = resp
    "CreatePresetResponse"
    "fixture/CreatePresetResponse"
    (Proxy :: Proxy CreatePreset)

listPresetsResponseTest :: ListPresetsResponse -> TestTree
listPresetsResponseTest = resp
    "ListPresetsResponse"
    "fixture/ListPresetsResponse"
    (Proxy :: Proxy ListPresets)

readPresetResponseTest :: ReadPresetResponse -> TestTree
readPresetResponseTest = resp
    "ReadPresetResponse"
    "fixture/ReadPresetResponse"
    (Proxy :: Proxy ReadPreset)

updatePipelineNotificationsResponseTest :: UpdatePipelineNotificationsResponse -> TestTree
updatePipelineNotificationsResponseTest = resp
    "UpdatePipelineNotificationsResponse"
    "fixture/UpdatePipelineNotificationsResponse"
    (Proxy :: Proxy UpdatePipelineNotifications)

readJobResponseTest :: ReadJobResponse -> TestTree
readJobResponseTest = resp
    "ReadJobResponse"
    "fixture/ReadJobResponse"
    (Proxy :: Proxy ReadJob)

readPipelineResponseTest :: ReadPipelineResponse -> TestTree
readPipelineResponseTest = resp
    "ReadPipelineResponse"
    "fixture/ReadPipelineResponse"
    (Proxy :: Proxy ReadPipeline)

createPipelineResponseTest :: CreatePipelineResponse -> TestTree
createPipelineResponseTest = resp
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse"
    (Proxy :: Proxy CreatePipeline)

listJobsByStatusResponseTest :: ListJobsByStatusResponse -> TestTree
listJobsByStatusResponseTest = resp
    "ListJobsByStatusResponse"
    "fixture/ListJobsByStatusResponse"
    (Proxy :: Proxy ListJobsByStatus)

cancelJobResponseTest :: CancelJobResponse -> TestTree
cancelJobResponseTest = resp
    "CancelJobResponse"
    "fixture/CancelJobResponse"
    (Proxy :: Proxy CancelJob)
