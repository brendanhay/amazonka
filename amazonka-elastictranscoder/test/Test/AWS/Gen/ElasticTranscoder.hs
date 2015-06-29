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
--         [ cancelJobTest $
--             cancelJob
--
--         , createJobTest $
--             createJob
--
--         , createPipelineTest $
--             createPipeline
--
--         , createPresetTest $
--             createPreset
--
--         , deletePipelineTest $
--             deletePipeline
--
--         , deletePresetTest $
--             deletePreset
--
--         , listJobsByPipelineTest $
--             listJobsByPipeline
--
--         , listJobsByStatusTest $
--             listJobsByStatus
--
--         , listPipelinesTest $
--             listPipelines
--
--         , listPresetsTest $
--             listPresets
--
--         , readJobTest $
--             readJob
--
--         , readPipelineTest $
--             readPipeline
--
--         , readPresetTest $
--             readPreset
--
--         , testRoleTest $
--             testRole
--
--         , updatePipelineTest $
--             updatePipeline
--
--         , updatePipelineNotificationsTest $
--             updatePipelineNotifications
--
--         , updatePipelineStatusTest $
--             updatePipelineStatus
--
--           ]

--     , testGroup "response"
--         [ cancelJobResponseTest $
--             cancelJobResponse
--
--         , createJobResponseTest $
--             createJobResponse
--
--         , createPipelineResponseTest $
--             createPipelineResponse
--
--         , createPresetResponseTest $
--             createPresetResponse
--
--         , deletePipelineResponseTest $
--             deletePipelineResponse
--
--         , deletePresetResponseTest $
--             deletePresetResponse
--
--         , listJobsByPipelineResponseTest $
--             listJobsByPipelineResponse
--
--         , listJobsByStatusResponseTest $
--             listJobsByStatusResponse
--
--         , listPipelinesResponseTest $
--             listPipelinesResponse
--
--         , listPresetsResponseTest $
--             listPresetsResponse
--
--         , readJobResponseTest $
--             readJobResponse
--
--         , readPipelineResponseTest $
--             readPipelineResponse
--
--         , readPresetResponseTest $
--             readPresetResponse
--
--         , testRoleResponseTest $
--             testRoleResponse
--
--         , updatePipelineResponseTest $
--             updatePipelineResponse
--
--         , updatePipelineNotificationsResponseTest $
--             updatePipelineNotificationsResponse
--
--         , updatePipelineStatusResponseTest $
--             updatePipelineStatusResponse
--
--           ]
--     ]

-- Requests

cancelJobTest :: CancelJob -> TestTree
cancelJobTest = undefined

createJobTest :: CreateJob -> TestTree
createJobTest = undefined

createPipelineTest :: CreatePipeline -> TestTree
createPipelineTest = undefined

createPresetTest :: CreatePreset -> TestTree
createPresetTest = undefined

deletePipelineTest :: DeletePipeline -> TestTree
deletePipelineTest = undefined

deletePresetTest :: DeletePreset -> TestTree
deletePresetTest = undefined

listJobsByPipelineTest :: ListJobsByPipeline -> TestTree
listJobsByPipelineTest = undefined

listJobsByStatusTest :: ListJobsByStatus -> TestTree
listJobsByStatusTest = undefined

listPipelinesTest :: ListPipelines -> TestTree
listPipelinesTest = undefined

listPresetsTest :: ListPresets -> TestTree
listPresetsTest = undefined

readJobTest :: ReadJob -> TestTree
readJobTest = undefined

readPipelineTest :: ReadPipeline -> TestTree
readPipelineTest = undefined

readPresetTest :: ReadPreset -> TestTree
readPresetTest = undefined

testRoleTest :: TestRole -> TestTree
testRoleTest = undefined

updatePipelineTest :: UpdatePipeline -> TestTree
updatePipelineTest = undefined

updatePipelineNotificationsTest :: UpdatePipelineNotifications -> TestTree
updatePipelineNotificationsTest = undefined

updatePipelineStatusTest :: UpdatePipelineStatus -> TestTree
updatePipelineStatusTest = undefined

-- Responses

cancelJobResponseTest :: CancelJobResponse -> TestTree
cancelJobResponseTest = resp
    "cancelJobResponse"
    "fixture/CancelJobResponse"
    (Proxy :: Proxy CancelJob)

createJobResponseTest :: CreateJobResponse -> TestTree
createJobResponseTest = resp
    "createJobResponse"
    "fixture/CreateJobResponse"
    (Proxy :: Proxy CreateJob)

createPipelineResponseTest :: CreatePipelineResponse -> TestTree
createPipelineResponseTest = resp
    "createPipelineResponse"
    "fixture/CreatePipelineResponse"
    (Proxy :: Proxy CreatePipeline)

createPresetResponseTest :: CreatePresetResponse -> TestTree
createPresetResponseTest = resp
    "createPresetResponse"
    "fixture/CreatePresetResponse"
    (Proxy :: Proxy CreatePreset)

deletePipelineResponseTest :: DeletePipelineResponse -> TestTree
deletePipelineResponseTest = resp
    "deletePipelineResponse"
    "fixture/DeletePipelineResponse"
    (Proxy :: Proxy DeletePipeline)

deletePresetResponseTest :: DeletePresetResponse -> TestTree
deletePresetResponseTest = resp
    "deletePresetResponse"
    "fixture/DeletePresetResponse"
    (Proxy :: Proxy DeletePreset)

listJobsByPipelineResponseTest :: ListJobsByPipelineResponse -> TestTree
listJobsByPipelineResponseTest = resp
    "listJobsByPipelineResponse"
    "fixture/ListJobsByPipelineResponse"
    (Proxy :: Proxy ListJobsByPipeline)

listJobsByStatusResponseTest :: ListJobsByStatusResponse -> TestTree
listJobsByStatusResponseTest = resp
    "listJobsByStatusResponse"
    "fixture/ListJobsByStatusResponse"
    (Proxy :: Proxy ListJobsByStatus)

listPipelinesResponseTest :: ListPipelinesResponse -> TestTree
listPipelinesResponseTest = resp
    "listPipelinesResponse"
    "fixture/ListPipelinesResponse"
    (Proxy :: Proxy ListPipelines)

listPresetsResponseTest :: ListPresetsResponse -> TestTree
listPresetsResponseTest = resp
    "listPresetsResponse"
    "fixture/ListPresetsResponse"
    (Proxy :: Proxy ListPresets)

readJobResponseTest :: ReadJobResponse -> TestTree
readJobResponseTest = resp
    "readJobResponse"
    "fixture/ReadJobResponse"
    (Proxy :: Proxy ReadJob)

readPipelineResponseTest :: ReadPipelineResponse -> TestTree
readPipelineResponseTest = resp
    "readPipelineResponse"
    "fixture/ReadPipelineResponse"
    (Proxy :: Proxy ReadPipeline)

readPresetResponseTest :: ReadPresetResponse -> TestTree
readPresetResponseTest = resp
    "readPresetResponse"
    "fixture/ReadPresetResponse"
    (Proxy :: Proxy ReadPreset)

testRoleResponseTest :: TestRoleResponse -> TestTree
testRoleResponseTest = resp
    "testRoleResponse"
    "fixture/TestRoleResponse"
    (Proxy :: Proxy TestRole)

updatePipelineResponseTest :: UpdatePipelineResponse -> TestTree
updatePipelineResponseTest = resp
    "updatePipelineResponse"
    "fixture/UpdatePipelineResponse"
    (Proxy :: Proxy UpdatePipeline)

updatePipelineNotificationsResponseTest :: UpdatePipelineNotificationsResponse -> TestTree
updatePipelineNotificationsResponseTest = resp
    "updatePipelineNotificationsResponse"
    "fixture/UpdatePipelineNotificationsResponse"
    (Proxy :: Proxy UpdatePipelineNotifications)

updatePipelineStatusResponseTest :: UpdatePipelineStatusResponse -> TestTree
updatePipelineStatusResponseTest = resp
    "updatePipelineStatusResponse"
    "fixture/UpdatePipelineStatusResponse"
    (Proxy :: Proxy UpdatePipelineStatus)
