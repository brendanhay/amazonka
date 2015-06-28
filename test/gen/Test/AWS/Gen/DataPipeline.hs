-- Module      : Test.AWS.Gen.DataPipeline
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

module Test.AWS.Gen.DataPipeline where

import           Data.Proxy
import           Network.AWS.DataPipeline
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describePipelinesTest $
--             describePipelines
--
--         , queryObjectsTest $
--             queryObjects
--
--         , removeTagsTest $
--             removeTags
--
--         , deletePipelineTest $
--             deletePipeline
--
--         , listPipelinesTest $
--             listPipelines
--
--         , getPipelineDefinitionTest $
--             getPipelineDefinition
--
--         , pollForTaskTest $
--             pollForTask
--
--         , evaluateExpressionTest $
--             evaluateExpression
--
--         , deactivatePipelineTest $
--             deactivatePipeline
--
--         , addTagsTest $
--             addTags
--
--         , describeObjectsTest $
--             describeObjects
--
--         , reportTaskRunnerHeartbeatTest $
--             reportTaskRunnerHeartbeat
--
--         , activatePipelineTest $
--             activatePipeline
--
--         , setTaskStatusTest $
--             setTaskStatus
--
--         , reportTaskProgressTest $
--             reportTaskProgress
--
--         , createPipelineTest $
--             createPipeline
--
--         , setStatusTest $
--             setStatus
--
--         , putPipelineDefinitionTest $
--             putPipelineDefinition
--
--         , validatePipelineDefinitionTest $
--             validatePipelineDefinition
--
--           ]

--     , testGroup "response"
--         [ describePipelinesResponseTest $
--             describePipelinesResponse
--
--         , queryObjectsResponseTest $
--             queryObjectsResponse
--
--         , removeTagsResponseTest $
--             removeTagsResponse
--
--         , deletePipelineResponseTest $
--             deletePipelineResponse
--
--         , listPipelinesResponseTest $
--             listPipelinesResponse
--
--         , getPipelineDefinitionResponseTest $
--             getPipelineDefinitionResponse
--
--         , pollForTaskResponseTest $
--             pollForTaskResponse
--
--         , evaluateExpressionResponseTest $
--             evaluateExpressionResponse
--
--         , deactivatePipelineResponseTest $
--             deactivatePipelineResponse
--
--         , addTagsResponseTest $
--             addTagsResponse
--
--         , describeObjectsResponseTest $
--             describeObjectsResponse
--
--         , reportTaskRunnerHeartbeatResponseTest $
--             reportTaskRunnerHeartbeatResponse
--
--         , activatePipelineResponseTest $
--             activatePipelineResponse
--
--         , setTaskStatusResponseTest $
--             setTaskStatusResponse
--
--         , reportTaskProgressResponseTest $
--             reportTaskProgressResponse
--
--         , createPipelineResponseTest $
--             createPipelineResponse
--
--         , setStatusResponseTest $
--             setStatusResponse
--
--         , putPipelineDefinitionResponseTest $
--             putPipelineDefinitionResponse
--
--         , validatePipelineDefinitionResponseTest $
--             validatePipelineDefinitionResponse
--
--           ]
--     ]

-- Requests

describePipelinesTest :: DescribePipelines -> TestTree
describePipelinesTest = undefined

queryObjectsTest :: QueryObjects -> TestTree
queryObjectsTest = undefined

removeTagsTest :: RemoveTags -> TestTree
removeTagsTest = undefined

deletePipelineTest :: DeletePipeline -> TestTree
deletePipelineTest = undefined

listPipelinesTest :: ListPipelines -> TestTree
listPipelinesTest = undefined

getPipelineDefinitionTest :: GetPipelineDefinition -> TestTree
getPipelineDefinitionTest = undefined

pollForTaskTest :: PollForTask -> TestTree
pollForTaskTest = undefined

evaluateExpressionTest :: EvaluateExpression -> TestTree
evaluateExpressionTest = undefined

deactivatePipelineTest :: DeactivatePipeline -> TestTree
deactivatePipelineTest = undefined

addTagsTest :: AddTags -> TestTree
addTagsTest = undefined

describeObjectsTest :: DescribeObjects -> TestTree
describeObjectsTest = undefined

reportTaskRunnerHeartbeatTest :: ReportTaskRunnerHeartbeat -> TestTree
reportTaskRunnerHeartbeatTest = undefined

activatePipelineTest :: ActivatePipeline -> TestTree
activatePipelineTest = undefined

setTaskStatusTest :: SetTaskStatus -> TestTree
setTaskStatusTest = undefined

reportTaskProgressTest :: ReportTaskProgress -> TestTree
reportTaskProgressTest = undefined

createPipelineTest :: CreatePipeline -> TestTree
createPipelineTest = undefined

setStatusTest :: SetStatus -> TestTree
setStatusTest = undefined

putPipelineDefinitionTest :: PutPipelineDefinition -> TestTree
putPipelineDefinitionTest = undefined

validatePipelineDefinitionTest :: ValidatePipelineDefinition -> TestTree
validatePipelineDefinitionTest = undefined

-- Responses

describePipelinesResponseTest :: DescribePipelinesResponse -> TestTree
describePipelinesResponseTest = resp
    "DescribePipelinesResponse"
    "fixture/DataPipeline/DescribePipelinesResponse"
    (Proxy :: Proxy DescribePipelines)

queryObjectsResponseTest :: QueryObjectsResponse -> TestTree
queryObjectsResponseTest = resp
    "QueryObjectsResponse"
    "fixture/DataPipeline/QueryObjectsResponse"
    (Proxy :: Proxy QueryObjects)

removeTagsResponseTest :: RemoveTagsResponse -> TestTree
removeTagsResponseTest = resp
    "RemoveTagsResponse"
    "fixture/DataPipeline/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

deletePipelineResponseTest :: DeletePipelineResponse -> TestTree
deletePipelineResponseTest = resp
    "DeletePipelineResponse"
    "fixture/DataPipeline/DeletePipelineResponse"
    (Proxy :: Proxy DeletePipeline)

listPipelinesResponseTest :: ListPipelinesResponse -> TestTree
listPipelinesResponseTest = resp
    "ListPipelinesResponse"
    "fixture/DataPipeline/ListPipelinesResponse"
    (Proxy :: Proxy ListPipelines)

getPipelineDefinitionResponseTest :: GetPipelineDefinitionResponse -> TestTree
getPipelineDefinitionResponseTest = resp
    "GetPipelineDefinitionResponse"
    "fixture/DataPipeline/GetPipelineDefinitionResponse"
    (Proxy :: Proxy GetPipelineDefinition)

pollForTaskResponseTest :: PollForTaskResponse -> TestTree
pollForTaskResponseTest = resp
    "PollForTaskResponse"
    "fixture/DataPipeline/PollForTaskResponse"
    (Proxy :: Proxy PollForTask)

evaluateExpressionResponseTest :: EvaluateExpressionResponse -> TestTree
evaluateExpressionResponseTest = resp
    "EvaluateExpressionResponse"
    "fixture/DataPipeline/EvaluateExpressionResponse"
    (Proxy :: Proxy EvaluateExpression)

deactivatePipelineResponseTest :: DeactivatePipelineResponse -> TestTree
deactivatePipelineResponseTest = resp
    "DeactivatePipelineResponse"
    "fixture/DataPipeline/DeactivatePipelineResponse"
    (Proxy :: Proxy DeactivatePipeline)

addTagsResponseTest :: AddTagsResponse -> TestTree
addTagsResponseTest = resp
    "AddTagsResponse"
    "fixture/DataPipeline/AddTagsResponse"
    (Proxy :: Proxy AddTags)

describeObjectsResponseTest :: DescribeObjectsResponse -> TestTree
describeObjectsResponseTest = resp
    "DescribeObjectsResponse"
    "fixture/DataPipeline/DescribeObjectsResponse"
    (Proxy :: Proxy DescribeObjects)

reportTaskRunnerHeartbeatResponseTest :: ReportTaskRunnerHeartbeatResponse -> TestTree
reportTaskRunnerHeartbeatResponseTest = resp
    "ReportTaskRunnerHeartbeatResponse"
    "fixture/DataPipeline/ReportTaskRunnerHeartbeatResponse"
    (Proxy :: Proxy ReportTaskRunnerHeartbeat)

activatePipelineResponseTest :: ActivatePipelineResponse -> TestTree
activatePipelineResponseTest = resp
    "ActivatePipelineResponse"
    "fixture/DataPipeline/ActivatePipelineResponse"
    (Proxy :: Proxy ActivatePipeline)

setTaskStatusResponseTest :: SetTaskStatusResponse -> TestTree
setTaskStatusResponseTest = resp
    "SetTaskStatusResponse"
    "fixture/DataPipeline/SetTaskStatusResponse"
    (Proxy :: Proxy SetTaskStatus)

reportTaskProgressResponseTest :: ReportTaskProgressResponse -> TestTree
reportTaskProgressResponseTest = resp
    "ReportTaskProgressResponse"
    "fixture/DataPipeline/ReportTaskProgressResponse"
    (Proxy :: Proxy ReportTaskProgress)

createPipelineResponseTest :: CreatePipelineResponse -> TestTree
createPipelineResponseTest = resp
    "CreatePipelineResponse"
    "fixture/DataPipeline/CreatePipelineResponse"
    (Proxy :: Proxy CreatePipeline)

setStatusResponseTest :: SetStatusResponse -> TestTree
setStatusResponseTest = resp
    "SetStatusResponse"
    "fixture/DataPipeline/SetStatusResponse"
    (Proxy :: Proxy SetStatus)

putPipelineDefinitionResponseTest :: PutPipelineDefinitionResponse -> TestTree
putPipelineDefinitionResponseTest = resp
    "PutPipelineDefinitionResponse"
    "fixture/DataPipeline/PutPipelineDefinitionResponse"
    (Proxy :: Proxy PutPipelineDefinition)

validatePipelineDefinitionResponseTest :: ValidatePipelineDefinitionResponse -> TestTree
validatePipelineDefinitionResponseTest = resp
    "ValidatePipelineDefinitionResponse"
    "fixture/DataPipeline/ValidatePipelineDefinitionResponse"
    (Proxy :: Proxy ValidatePipelineDefinition)
