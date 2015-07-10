{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DataPipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
testDescribePipelines = req
    "DescribePipelines"
    "fixture/DescribePipelines"

testQueryObjects :: QueryObjects -> TestTree
testQueryObjects = req
    "QueryObjects"
    "fixture/QueryObjects"

testRemoveTags :: RemoveTags -> TestTree
testRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags"

testDeletePipeline :: DeletePipeline -> TestTree
testDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline"

testListPipelines :: ListPipelines -> TestTree
testListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines"

testGetPipelineDefinition :: GetPipelineDefinition -> TestTree
testGetPipelineDefinition = req
    "GetPipelineDefinition"
    "fixture/GetPipelineDefinition"

testPollForTask :: PollForTask -> TestTree
testPollForTask = req
    "PollForTask"
    "fixture/PollForTask"

testEvaluateExpression :: EvaluateExpression -> TestTree
testEvaluateExpression = req
    "EvaluateExpression"
    "fixture/EvaluateExpression"

testDeactivatePipeline :: DeactivatePipeline -> TestTree
testDeactivatePipeline = req
    "DeactivatePipeline"
    "fixture/DeactivatePipeline"

testAddTags :: AddTags -> TestTree
testAddTags = req
    "AddTags"
    "fixture/AddTags"

testDescribeObjects :: DescribeObjects -> TestTree
testDescribeObjects = req
    "DescribeObjects"
    "fixture/DescribeObjects"

testReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeat -> TestTree
testReportTaskRunnerHeartbeat = req
    "ReportTaskRunnerHeartbeat"
    "fixture/ReportTaskRunnerHeartbeat"

testActivatePipeline :: ActivatePipeline -> TestTree
testActivatePipeline = req
    "ActivatePipeline"
    "fixture/ActivatePipeline"

testSetTaskStatus :: SetTaskStatus -> TestTree
testSetTaskStatus = req
    "SetTaskStatus"
    "fixture/SetTaskStatus"

testReportTaskProgress :: ReportTaskProgress -> TestTree
testReportTaskProgress = req
    "ReportTaskProgress"
    "fixture/ReportTaskProgress"

testCreatePipeline :: CreatePipeline -> TestTree
testCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline"

testSetStatus :: SetStatus -> TestTree
testSetStatus = req
    "SetStatus"
    "fixture/SetStatus"

testPutPipelineDefinition :: PutPipelineDefinition -> TestTree
testPutPipelineDefinition = req
    "PutPipelineDefinition"
    "fixture/PutPipelineDefinition"

testValidatePipelineDefinition :: ValidatePipelineDefinition -> TestTree
testValidatePipelineDefinition = req
    "ValidatePipelineDefinition"
    "fixture/ValidatePipelineDefinition"

-- Responses

testDescribePipelinesResponse :: DescribePipelinesResponse -> TestTree
testDescribePipelinesResponse = res
    "DescribePipelinesResponse"
    "fixture/DescribePipelinesResponse"
    (Proxy :: Proxy DescribePipelines)

testQueryObjectsResponse :: QueryObjectsResponse -> TestTree
testQueryObjectsResponse = res
    "QueryObjectsResponse"
    "fixture/QueryObjectsResponse"
    (Proxy :: Proxy QueryObjects)

testRemoveTagsResponse :: RemoveTagsResponse -> TestTree
testRemoveTagsResponse = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

testDeletePipelineResponse :: DeletePipelineResponse -> TestTree
testDeletePipelineResponse = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse"
    (Proxy :: Proxy DeletePipeline)

testListPipelinesResponse :: ListPipelinesResponse -> TestTree
testListPipelinesResponse = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse"
    (Proxy :: Proxy ListPipelines)

testGetPipelineDefinitionResponse :: GetPipelineDefinitionResponse -> TestTree
testGetPipelineDefinitionResponse = res
    "GetPipelineDefinitionResponse"
    "fixture/GetPipelineDefinitionResponse"
    (Proxy :: Proxy GetPipelineDefinition)

testPollForTaskResponse :: PollForTaskResponse -> TestTree
testPollForTaskResponse = res
    "PollForTaskResponse"
    "fixture/PollForTaskResponse"
    (Proxy :: Proxy PollForTask)

testEvaluateExpressionResponse :: EvaluateExpressionResponse -> TestTree
testEvaluateExpressionResponse = res
    "EvaluateExpressionResponse"
    "fixture/EvaluateExpressionResponse"
    (Proxy :: Proxy EvaluateExpression)

testDeactivatePipelineResponse :: DeactivatePipelineResponse -> TestTree
testDeactivatePipelineResponse = res
    "DeactivatePipelineResponse"
    "fixture/DeactivatePipelineResponse"
    (Proxy :: Proxy DeactivatePipeline)

testAddTagsResponse :: AddTagsResponse -> TestTree
testAddTagsResponse = res
    "AddTagsResponse"
    "fixture/AddTagsResponse"
    (Proxy :: Proxy AddTags)

testDescribeObjectsResponse :: DescribeObjectsResponse -> TestTree
testDescribeObjectsResponse = res
    "DescribeObjectsResponse"
    "fixture/DescribeObjectsResponse"
    (Proxy :: Proxy DescribeObjects)

testReportTaskRunnerHeartbeatResponse :: ReportTaskRunnerHeartbeatResponse -> TestTree
testReportTaskRunnerHeartbeatResponse = res
    "ReportTaskRunnerHeartbeatResponse"
    "fixture/ReportTaskRunnerHeartbeatResponse"
    (Proxy :: Proxy ReportTaskRunnerHeartbeat)

testActivatePipelineResponse :: ActivatePipelineResponse -> TestTree
testActivatePipelineResponse = res
    "ActivatePipelineResponse"
    "fixture/ActivatePipelineResponse"
    (Proxy :: Proxy ActivatePipeline)

testSetTaskStatusResponse :: SetTaskStatusResponse -> TestTree
testSetTaskStatusResponse = res
    "SetTaskStatusResponse"
    "fixture/SetTaskStatusResponse"
    (Proxy :: Proxy SetTaskStatus)

testReportTaskProgressResponse :: ReportTaskProgressResponse -> TestTree
testReportTaskProgressResponse = res
    "ReportTaskProgressResponse"
    "fixture/ReportTaskProgressResponse"
    (Proxy :: Proxy ReportTaskProgress)

testCreatePipelineResponse :: CreatePipelineResponse -> TestTree
testCreatePipelineResponse = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse"
    (Proxy :: Proxy CreatePipeline)

testSetStatusResponse :: SetStatusResponse -> TestTree
testSetStatusResponse = res
    "SetStatusResponse"
    "fixture/SetStatusResponse"
    (Proxy :: Proxy SetStatus)

testPutPipelineDefinitionResponse :: PutPipelineDefinitionResponse -> TestTree
testPutPipelineDefinitionResponse = res
    "PutPipelineDefinitionResponse"
    "fixture/PutPipelineDefinitionResponse"
    (Proxy :: Proxy PutPipelineDefinition)

testValidatePipelineDefinitionResponse :: ValidatePipelineDefinitionResponse -> TestTree
testValidatePipelineDefinitionResponse = res
    "ValidatePipelineDefinitionResponse"
    "fixture/ValidatePipelineDefinitionResponse"
    (Proxy :: Proxy ValidatePipelineDefinition)

instance Out ActivatePipeline
instance Out ActivatePipelineResponse
instance Out AddTags
instance Out AddTagsResponse
instance Out CreatePipeline
instance Out CreatePipelineResponse
instance Out DeactivatePipeline
instance Out DeactivatePipelineResponse
instance Out DeletePipeline
instance Out DeletePipelineResponse
instance Out DescribeObjects
instance Out DescribeObjectsResponse
instance Out DescribePipelines
instance Out DescribePipelinesResponse
instance Out EvaluateExpression
instance Out EvaluateExpressionResponse
instance Out Field
instance Out GetPipelineDefinition
instance Out GetPipelineDefinitionResponse
instance Out InstanceIdentity
instance Out ListPipelines
instance Out ListPipelinesResponse
instance Out Operator
instance Out OperatorType
instance Out ParameterAttribute
instance Out ParameterObject
instance Out ParameterValue
instance Out PipelineDescription
instance Out PipelineIdName
instance Out PipelineObject
instance Out PollForTask
instance Out PollForTaskResponse
instance Out PutPipelineDefinition
instance Out PutPipelineDefinitionResponse
instance Out Query
instance Out QueryObjects
instance Out QueryObjectsResponse
instance Out RemoveTags
instance Out RemoveTagsResponse
instance Out ReportTaskProgress
instance Out ReportTaskProgressResponse
instance Out ReportTaskRunnerHeartbeat
instance Out ReportTaskRunnerHeartbeatResponse
instance Out Selector
instance Out SetStatus
instance Out SetStatusResponse
instance Out SetTaskStatus
instance Out SetTaskStatusResponse
instance Out Tag
instance Out TaskObject
instance Out TaskStatus
instance Out ValidatePipelineDefinition
instance Out ValidatePipelineDefinitionResponse
instance Out ValidationError
instance Out ValidationWarning
