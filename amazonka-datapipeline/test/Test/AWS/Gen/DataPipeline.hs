-- Module      : Test.AWS.Gen.DataPipeline
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

module Test.AWS.Gen.DataPipeline where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.DataPipeline

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribePipelines $
--             describePipelines
--
--         , testQueryObjects $
--             queryObjects
--
--         , testRemoveTags $
--             removeTags
--
--         , testDeletePipeline $
--             deletePipeline
--
--         , testListPipelines $
--             listPipelines
--
--         , testGetPipelineDefinition $
--             getPipelineDefinition
--
--         , testPollForTask $
--             pollForTask
--
--         , testEvaluateExpression $
--             evaluateExpression
--
--         , testDeactivatePipeline $
--             deactivatePipeline
--
--         , testAddTags $
--             addTags
--
--         , testDescribeObjects $
--             describeObjects
--
--         , testReportTaskRunnerHeartbeat $
--             reportTaskRunnerHeartbeat
--
--         , testActivatePipeline $
--             activatePipeline
--
--         , testSetTaskStatus $
--             setTaskStatus
--
--         , testReportTaskProgress $
--             reportTaskProgress
--
--         , testCreatePipeline $
--             createPipeline
--
--         , testSetStatus $
--             setStatus
--
--         , testPutPipelineDefinition $
--             putPipelineDefinition
--
--         , testValidatePipelineDefinition $
--             validatePipelineDefinition
--
--           ]

--     , testGroup "response"
--         [ testDescribePipelinesResponse $
--             describePipelinesResponse
--
--         , testQueryObjectsResponse $
--             queryObjectsResponse
--
--         , testRemoveTagsResponse $
--             removeTagsResponse
--
--         , testDeletePipelineResponse $
--             deletePipelineResponse
--
--         , testListPipelinesResponse $
--             listPipelinesResponse
--
--         , testGetPipelineDefinitionResponse $
--             getPipelineDefinitionResponse
--
--         , testPollForTaskResponse $
--             pollForTaskResponse
--
--         , testEvaluateExpressionResponse $
--             evaluateExpressionResponse
--
--         , testDeactivatePipelineResponse $
--             deactivatePipelineResponse
--
--         , testAddTagsResponse $
--             addTagsResponse
--
--         , testDescribeObjectsResponse $
--             describeObjectsResponse
--
--         , testReportTaskRunnerHeartbeatResponse $
--             reportTaskRunnerHeartbeatResponse
--
--         , testActivatePipelineResponse $
--             activatePipelineResponse
--
--         , testSetTaskStatusResponse $
--             setTaskStatusResponse
--
--         , testReportTaskProgressResponse $
--             reportTaskProgressResponse
--
--         , testCreatePipelineResponse $
--             createPipelineResponse
--
--         , testSetStatusResponse $
--             setStatusResponse
--
--         , testPutPipelineDefinitionResponse $
--             putPipelineDefinitionResponse
--
--         , testValidatePipelineDefinitionResponse $
--             validatePipelineDefinitionResponse
--
--           ]
--     ]

-- Requests

testDescribePipelines :: DescribePipelines -> TestTree
testDescribePipelines = undefined

testQueryObjects :: QueryObjects -> TestTree
testQueryObjects = undefined

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = undefined

testDeletePipeline :: DeletePipeline -> TestTree
testDeletePipeline = undefined

testListPipelines :: ListPipelines -> TestTree
testListPipelines = undefined

testGetPipelineDefinition :: GetPipelineDefinition -> TestTree
testGetPipelineDefinition = undefined

testPollForTask :: PollForTask -> TestTree
testPollForTask = undefined

testEvaluateExpression :: EvaluateExpression -> TestTree
testEvaluateExpression = undefined

testDeactivatePipeline :: DeactivatePipeline -> TestTree
testDeactivatePipeline = undefined

testAddTags :: AddTags -> TestTree
testAddTags = undefined

testDescribeObjects :: DescribeObjects -> TestTree
testDescribeObjects = undefined

testReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeat -> TestTree
testReportTaskRunnerHeartbeat = undefined

testActivatePipeline :: ActivatePipeline -> TestTree
testActivatePipeline = undefined

testSetTaskStatus :: SetTaskStatus -> TestTree
testSetTaskStatus = undefined

testReportTaskProgress :: ReportTaskProgress -> TestTree
testReportTaskProgress = undefined

testCreatePipeline :: CreatePipeline -> TestTree
testCreatePipeline = undefined

testSetStatus :: SetStatus -> TestTree
testSetStatus = undefined

testPutPipelineDefinition :: PutPipelineDefinition -> TestTree
testPutPipelineDefinition = undefined

testValidatePipelineDefinition :: ValidatePipelineDefinition -> TestTree
testValidatePipelineDefinition = undefined

-- Responses

testDescribePipelinesResponse :: DescribePipelinesResponse -> TestTree
testDescribePipelinesResponse = resp
    "DescribePipelinesResponse"
    "fixture/DescribePipelinesResponse"
    (Proxy :: Proxy DescribePipelines)

testQueryObjectsResponse :: QueryObjectsResponse -> TestTree
testQueryObjectsResponse = resp
    "QueryObjectsResponse"
    "fixture/QueryObjectsResponse"
    (Proxy :: Proxy QueryObjects)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = resp
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

testDeletePipelineResponse :: DeletePipelineResponse -> TestTree
testDeletePipelineResponse = resp
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse"
    (Proxy :: Proxy DeletePipeline)

testListPipelinesResponse :: ListPipelinesResponse -> TestTree
testListPipelinesResponse = resp
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse"
    (Proxy :: Proxy ListPipelines)

testGetPipelineDefinitionResponse :: GetPipelineDefinitionResponse -> TestTree
testGetPipelineDefinitionResponse = resp
    "GetPipelineDefinitionResponse"
    "fixture/GetPipelineDefinitionResponse"
    (Proxy :: Proxy GetPipelineDefinition)

testPollForTaskResponse :: PollForTaskResponse -> TestTree
testPollForTaskResponse = resp
    "PollForTaskResponse"
    "fixture/PollForTaskResponse"
    (Proxy :: Proxy PollForTask)

testEvaluateExpressionResponse :: EvaluateExpressionResponse -> TestTree
testEvaluateExpressionResponse = resp
    "EvaluateExpressionResponse"
    "fixture/EvaluateExpressionResponse"
    (Proxy :: Proxy EvaluateExpression)

testDeactivatePipelineResponse :: DeactivatePipelineResponse -> TestTree
testDeactivatePipelineResponse = resp
    "DeactivatePipelineResponse"
    "fixture/DeactivatePipelineResponse"
    (Proxy :: Proxy DeactivatePipeline)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = resp
    "AddTagsResponse"
    "fixture/AddTagsResponse"
    (Proxy :: Proxy AddTags)

testDescribeObjectsResponse :: DescribeObjectsResponse -> TestTree
testDescribeObjectsResponse = resp
    "DescribeObjectsResponse"
    "fixture/DescribeObjectsResponse"
    (Proxy :: Proxy DescribeObjects)

testReportTaskRunnerHeartbeatResponse :: ReportTaskRunnerHeartbeatResponse -> TestTree
testReportTaskRunnerHeartbeatResponse = resp
    "ReportTaskRunnerHeartbeatResponse"
    "fixture/ReportTaskRunnerHeartbeatResponse"
    (Proxy :: Proxy ReportTaskRunnerHeartbeat)

testActivatePipelineResponse :: ActivatePipelineResponse -> TestTree
testActivatePipelineResponse = resp
    "ActivatePipelineResponse"
    "fixture/ActivatePipelineResponse"
    (Proxy :: Proxy ActivatePipeline)

testSetTaskStatusResponse :: SetTaskStatusResponse -> TestTree
testSetTaskStatusResponse = resp
    "SetTaskStatusResponse"
    "fixture/SetTaskStatusResponse"
    (Proxy :: Proxy SetTaskStatus)

testReportTaskProgressResponse :: ReportTaskProgressResponse -> TestTree
testReportTaskProgressResponse = resp
    "ReportTaskProgressResponse"
    "fixture/ReportTaskProgressResponse"
    (Proxy :: Proxy ReportTaskProgress)

testCreatePipelineResponse :: CreatePipelineResponse -> TestTree
testCreatePipelineResponse = resp
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse"
    (Proxy :: Proxy CreatePipeline)

testSetStatusResponse :: SetStatusResponse -> TestTree
testSetStatusResponse = resp
    "SetStatusResponse"
    "fixture/SetStatusResponse"
    (Proxy :: Proxy SetStatus)

testPutPipelineDefinitionResponse :: PutPipelineDefinitionResponse -> TestTree
testPutPipelineDefinitionResponse = resp
    "PutPipelineDefinitionResponse"
    "fixture/PutPipelineDefinitionResponse"
    (Proxy :: Proxy PutPipelineDefinition)

testValidatePipelineDefinitionResponse :: ValidatePipelineDefinitionResponse -> TestTree
testValidatePipelineDefinitionResponse = resp
    "ValidatePipelineDefinitionResponse"
    "fixture/ValidatePipelineDefinitionResponse"
    (Proxy :: Proxy ValidatePipelineDefinition)
