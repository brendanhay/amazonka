{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DataPipeline
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DataPipeline where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.DataPipeline
import Test.AWS.DataPipeline.Internal

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
--         , testEvaluateExpression $
--             evaluateExpression
--
--         , testGetPipelineDefinition $
--             getPipelineDefinition
--
--         , testPollForTask $
--             pollForTask
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
--         , testSetStatus $
--             setStatus
--
--         , testReportTaskProgress $
--             reportTaskProgress
--
--         , testCreatePipeline $
--             createPipeline
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
--         , testEvaluateExpressionResponse $
--             evaluateExpressionResponse
--
--         , testGetPipelineDefinitionResponse $
--             getPipelineDefinitionResponse
--
--         , testPollForTaskResponse $
--             pollForTaskResponse
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
--         , testSetStatusResponse $
--             setStatusResponse
--
--         , testReportTaskProgressResponse $
--             reportTaskProgressResponse
--
--         , testCreatePipelineResponse $
--             createPipelineResponse
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
testDescribePipelines = req
    "DescribePipelines"
    "fixture/DescribePipelines.yaml"

testQueryObjects :: QueryObjects -> TestTree
testQueryObjects = req
    "QueryObjects"
    "fixture/QueryObjects.yaml"

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

testDeletePipeline :: DeletePipeline -> TestTree
testDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

testListPipelines :: ListPipelines -> TestTree
testListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

testEvaluateExpression :: EvaluateExpression -> TestTree
testEvaluateExpression = req
    "EvaluateExpression"
    "fixture/EvaluateExpression.yaml"

testGetPipelineDefinition :: GetPipelineDefinition -> TestTree
testGetPipelineDefinition = req
    "GetPipelineDefinition"
    "fixture/GetPipelineDefinition.yaml"

testPollForTask :: PollForTask -> TestTree
testPollForTask = req
    "PollForTask"
    "fixture/PollForTask.yaml"

testDeactivatePipeline :: DeactivatePipeline -> TestTree
testDeactivatePipeline = req
    "DeactivatePipeline"
    "fixture/DeactivatePipeline.yaml"

testAddTags :: AddTags -> TestTree
testAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

testDescribeObjects :: DescribeObjects -> TestTree
testDescribeObjects = req
    "DescribeObjects"
    "fixture/DescribeObjects.yaml"

testReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeat -> TestTree
testReportTaskRunnerHeartbeat = req
    "ReportTaskRunnerHeartbeat"
    "fixture/ReportTaskRunnerHeartbeat.yaml"

testActivatePipeline :: ActivatePipeline -> TestTree
testActivatePipeline = req
    "ActivatePipeline"
    "fixture/ActivatePipeline.yaml"

testSetTaskStatus :: SetTaskStatus -> TestTree
testSetTaskStatus = req
    "SetTaskStatus"
    "fixture/SetTaskStatus.yaml"

testSetStatus :: SetStatus -> TestTree
testSetStatus = req
    "SetStatus"
    "fixture/SetStatus.yaml"

testReportTaskProgress :: ReportTaskProgress -> TestTree
testReportTaskProgress = req
    "ReportTaskProgress"
    "fixture/ReportTaskProgress.yaml"

testCreatePipeline :: CreatePipeline -> TestTree
testCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

testPutPipelineDefinition :: PutPipelineDefinition -> TestTree
testPutPipelineDefinition = req
    "PutPipelineDefinition"
    "fixture/PutPipelineDefinition.yaml"

testValidatePipelineDefinition :: ValidatePipelineDefinition -> TestTree
testValidatePipelineDefinition = req
    "ValidatePipelineDefinition"
    "fixture/ValidatePipelineDefinition.yaml"

-- Responses

testDescribePipelinesResponse :: DescribePipelinesResponse -> TestTree
testDescribePipelinesResponse = res
    "DescribePipelinesResponse"
    "fixture/DescribePipelinesResponse.proto"
    dataPipeline
    (Proxy :: Proxy DescribePipelines)

testQueryObjectsResponse :: QueryObjectsResponse -> TestTree
testQueryObjectsResponse = res
    "QueryObjectsResponse"
    "fixture/QueryObjectsResponse.proto"
    dataPipeline
    (Proxy :: Proxy QueryObjects)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    dataPipeline
    (Proxy :: Proxy RemoveTags)

testDeletePipelineResponse :: DeletePipelineResponse -> TestTree
testDeletePipelineResponse = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    dataPipeline
    (Proxy :: Proxy DeletePipeline)

testListPipelinesResponse :: ListPipelinesResponse -> TestTree
testListPipelinesResponse = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    dataPipeline
    (Proxy :: Proxy ListPipelines)

testEvaluateExpressionResponse :: EvaluateExpressionResponse -> TestTree
testEvaluateExpressionResponse = res
    "EvaluateExpressionResponse"
    "fixture/EvaluateExpressionResponse.proto"
    dataPipeline
    (Proxy :: Proxy EvaluateExpression)

testGetPipelineDefinitionResponse :: GetPipelineDefinitionResponse -> TestTree
testGetPipelineDefinitionResponse = res
    "GetPipelineDefinitionResponse"
    "fixture/GetPipelineDefinitionResponse.proto"
    dataPipeline
    (Proxy :: Proxy GetPipelineDefinition)

testPollForTaskResponse :: PollForTaskResponse -> TestTree
testPollForTaskResponse = res
    "PollForTaskResponse"
    "fixture/PollForTaskResponse.proto"
    dataPipeline
    (Proxy :: Proxy PollForTask)

testDeactivatePipelineResponse :: DeactivatePipelineResponse -> TestTree
testDeactivatePipelineResponse = res
    "DeactivatePipelineResponse"
    "fixture/DeactivatePipelineResponse.proto"
    dataPipeline
    (Proxy :: Proxy DeactivatePipeline)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    dataPipeline
    (Proxy :: Proxy AddTags)

testDescribeObjectsResponse :: DescribeObjectsResponse -> TestTree
testDescribeObjectsResponse = res
    "DescribeObjectsResponse"
    "fixture/DescribeObjectsResponse.proto"
    dataPipeline
    (Proxy :: Proxy DescribeObjects)

testReportTaskRunnerHeartbeatResponse :: ReportTaskRunnerHeartbeatResponse -> TestTree
testReportTaskRunnerHeartbeatResponse = res
    "ReportTaskRunnerHeartbeatResponse"
    "fixture/ReportTaskRunnerHeartbeatResponse.proto"
    dataPipeline
    (Proxy :: Proxy ReportTaskRunnerHeartbeat)

testActivatePipelineResponse :: ActivatePipelineResponse -> TestTree
testActivatePipelineResponse = res
    "ActivatePipelineResponse"
    "fixture/ActivatePipelineResponse.proto"
    dataPipeline
    (Proxy :: Proxy ActivatePipeline)

testSetTaskStatusResponse :: SetTaskStatusResponse -> TestTree
testSetTaskStatusResponse = res
    "SetTaskStatusResponse"
    "fixture/SetTaskStatusResponse.proto"
    dataPipeline
    (Proxy :: Proxy SetTaskStatus)

testSetStatusResponse :: SetStatusResponse -> TestTree
testSetStatusResponse = res
    "SetStatusResponse"
    "fixture/SetStatusResponse.proto"
    dataPipeline
    (Proxy :: Proxy SetStatus)

testReportTaskProgressResponse :: ReportTaskProgressResponse -> TestTree
testReportTaskProgressResponse = res
    "ReportTaskProgressResponse"
    "fixture/ReportTaskProgressResponse.proto"
    dataPipeline
    (Proxy :: Proxy ReportTaskProgress)

testCreatePipelineResponse :: CreatePipelineResponse -> TestTree
testCreatePipelineResponse = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    dataPipeline
    (Proxy :: Proxy CreatePipeline)

testPutPipelineDefinitionResponse :: PutPipelineDefinitionResponse -> TestTree
testPutPipelineDefinitionResponse = res
    "PutPipelineDefinitionResponse"
    "fixture/PutPipelineDefinitionResponse.proto"
    dataPipeline
    (Proxy :: Proxy PutPipelineDefinition)

testValidatePipelineDefinitionResponse :: ValidatePipelineDefinitionResponse -> TestTree
testValidatePipelineDefinitionResponse = res
    "ValidatePipelineDefinitionResponse"
    "fixture/ValidatePipelineDefinitionResponse.proto"
    dataPipeline
    (Proxy :: Proxy ValidatePipelineDefinition)
