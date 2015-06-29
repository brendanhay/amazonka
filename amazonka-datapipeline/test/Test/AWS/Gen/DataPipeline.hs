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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.DataPipeline

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ activatePipelineTest $
--             activatePipeline
--
--         , addTagsTest $
--             addTags
--
--         , createPipelineTest $
--             createPipeline
--
--         , deactivatePipelineTest $
--             deactivatePipeline
--
--         , deletePipelineTest $
--             deletePipeline
--
--         , describeObjectsTest $
--             describeObjects
--
--         , describePipelinesTest $
--             describePipelines
--
--         , evaluateExpressionTest $
--             evaluateExpression
--
--         , getPipelineDefinitionTest $
--             getPipelineDefinition
--
--         , listPipelinesTest $
--             listPipelines
--
--         , pollForTaskTest $
--             pollForTask
--
--         , putPipelineDefinitionTest $
--             putPipelineDefinition
--
--         , queryObjectsTest $
--             queryObjects
--
--         , removeTagsTest $
--             removeTags
--
--         , reportTaskProgressTest $
--             reportTaskProgress
--
--         , reportTaskRunnerHeartbeatTest $
--             reportTaskRunnerHeartbeat
--
--         , setStatusTest $
--             setStatus
--
--         , setTaskStatusTest $
--             setTaskStatus
--
--         , validatePipelineDefinitionTest $
--             validatePipelineDefinition
--
--           ]

--     , testGroup "response"
--         [ activatePipelineResponseTest $
--             activatePipelineResponse
--
--         , addTagsResponseTest $
--             addTagsResponse
--
--         , createPipelineResponseTest $
--             createPipelineResponse
--
--         , deactivatePipelineResponseTest $
--             deactivatePipelineResponse
--
--         , deletePipelineResponseTest $
--             deletePipelineResponse
--
--         , describeObjectsResponseTest $
--             describeObjectsResponse
--
--         , describePipelinesResponseTest $
--             describePipelinesResponse
--
--         , evaluateExpressionResponseTest $
--             evaluateExpressionResponse
--
--         , getPipelineDefinitionResponseTest $
--             getPipelineDefinitionResponse
--
--         , listPipelinesResponseTest $
--             listPipelinesResponse
--
--         , pollForTaskResponseTest $
--             pollForTaskResponse
--
--         , putPipelineDefinitionResponseTest $
--             putPipelineDefinitionResponse
--
--         , queryObjectsResponseTest $
--             queryObjectsResponse
--
--         , removeTagsResponseTest $
--             removeTagsResponse
--
--         , reportTaskProgressResponseTest $
--             reportTaskProgressResponse
--
--         , reportTaskRunnerHeartbeatResponseTest $
--             reportTaskRunnerHeartbeatResponse
--
--         , setStatusResponseTest $
--             setStatusResponse
--
--         , setTaskStatusResponseTest $
--             setTaskStatusResponse
--
--         , validatePipelineDefinitionResponseTest $
--             validatePipelineDefinitionResponse
--
--           ]
--     ]

-- Requests

activatePipelineTest :: ActivatePipeline -> TestTree
activatePipelineTest = undefined

addTagsTest :: AddTags -> TestTree
addTagsTest = undefined

createPipelineTest :: CreatePipeline -> TestTree
createPipelineTest = undefined

deactivatePipelineTest :: DeactivatePipeline -> TestTree
deactivatePipelineTest = undefined

deletePipelineTest :: DeletePipeline -> TestTree
deletePipelineTest = undefined

describeObjectsTest :: DescribeObjects -> TestTree
describeObjectsTest = undefined

describePipelinesTest :: DescribePipelines -> TestTree
describePipelinesTest = undefined

evaluateExpressionTest :: EvaluateExpression -> TestTree
evaluateExpressionTest = undefined

getPipelineDefinitionTest :: GetPipelineDefinition -> TestTree
getPipelineDefinitionTest = undefined

listPipelinesTest :: ListPipelines -> TestTree
listPipelinesTest = undefined

pollForTaskTest :: PollForTask -> TestTree
pollForTaskTest = undefined

putPipelineDefinitionTest :: PutPipelineDefinition -> TestTree
putPipelineDefinitionTest = undefined

queryObjectsTest :: QueryObjects -> TestTree
queryObjectsTest = undefined

removeTagsTest :: RemoveTags -> TestTree
removeTagsTest = undefined

reportTaskProgressTest :: ReportTaskProgress -> TestTree
reportTaskProgressTest = undefined

reportTaskRunnerHeartbeatTest :: ReportTaskRunnerHeartbeat -> TestTree
reportTaskRunnerHeartbeatTest = undefined

setStatusTest :: SetStatus -> TestTree
setStatusTest = undefined

setTaskStatusTest :: SetTaskStatus -> TestTree
setTaskStatusTest = undefined

validatePipelineDefinitionTest :: ValidatePipelineDefinition -> TestTree
validatePipelineDefinitionTest = undefined

-- Responses

activatePipelineResponseTest :: ActivatePipelineResponse -> TestTree
activatePipelineResponseTest = resp
    "activatePipelineResponse"
    "fixture/ActivatePipelineResponse"
    (Proxy :: Proxy ActivatePipeline)

addTagsResponseTest :: AddTagsResponse -> TestTree
addTagsResponseTest = resp
    "addTagsResponse"
    "fixture/AddTagsResponse"
    (Proxy :: Proxy AddTags)

createPipelineResponseTest :: CreatePipelineResponse -> TestTree
createPipelineResponseTest = resp
    "createPipelineResponse"
    "fixture/CreatePipelineResponse"
    (Proxy :: Proxy CreatePipeline)

deactivatePipelineResponseTest :: DeactivatePipelineResponse -> TestTree
deactivatePipelineResponseTest = resp
    "deactivatePipelineResponse"
    "fixture/DeactivatePipelineResponse"
    (Proxy :: Proxy DeactivatePipeline)

deletePipelineResponseTest :: DeletePipelineResponse -> TestTree
deletePipelineResponseTest = resp
    "deletePipelineResponse"
    "fixture/DeletePipelineResponse"
    (Proxy :: Proxy DeletePipeline)

describeObjectsResponseTest :: DescribeObjectsResponse -> TestTree
describeObjectsResponseTest = resp
    "describeObjectsResponse"
    "fixture/DescribeObjectsResponse"
    (Proxy :: Proxy DescribeObjects)

describePipelinesResponseTest :: DescribePipelinesResponse -> TestTree
describePipelinesResponseTest = resp
    "describePipelinesResponse"
    "fixture/DescribePipelinesResponse"
    (Proxy :: Proxy DescribePipelines)

evaluateExpressionResponseTest :: EvaluateExpressionResponse -> TestTree
evaluateExpressionResponseTest = resp
    "evaluateExpressionResponse"
    "fixture/EvaluateExpressionResponse"
    (Proxy :: Proxy EvaluateExpression)

getPipelineDefinitionResponseTest :: GetPipelineDefinitionResponse -> TestTree
getPipelineDefinitionResponseTest = resp
    "getPipelineDefinitionResponse"
    "fixture/GetPipelineDefinitionResponse"
    (Proxy :: Proxy GetPipelineDefinition)

listPipelinesResponseTest :: ListPipelinesResponse -> TestTree
listPipelinesResponseTest = resp
    "listPipelinesResponse"
    "fixture/ListPipelinesResponse"
    (Proxy :: Proxy ListPipelines)

pollForTaskResponseTest :: PollForTaskResponse -> TestTree
pollForTaskResponseTest = resp
    "pollForTaskResponse"
    "fixture/PollForTaskResponse"
    (Proxy :: Proxy PollForTask)

putPipelineDefinitionResponseTest :: PutPipelineDefinitionResponse -> TestTree
putPipelineDefinitionResponseTest = resp
    "putPipelineDefinitionResponse"
    "fixture/PutPipelineDefinitionResponse"
    (Proxy :: Proxy PutPipelineDefinition)

queryObjectsResponseTest :: QueryObjectsResponse -> TestTree
queryObjectsResponseTest = resp
    "queryObjectsResponse"
    "fixture/QueryObjectsResponse"
    (Proxy :: Proxy QueryObjects)

removeTagsResponseTest :: RemoveTagsResponse -> TestTree
removeTagsResponseTest = resp
    "removeTagsResponse"
    "fixture/RemoveTagsResponse"
    (Proxy :: Proxy RemoveTags)

reportTaskProgressResponseTest :: ReportTaskProgressResponse -> TestTree
reportTaskProgressResponseTest = resp
    "reportTaskProgressResponse"
    "fixture/ReportTaskProgressResponse"
    (Proxy :: Proxy ReportTaskProgress)

reportTaskRunnerHeartbeatResponseTest :: ReportTaskRunnerHeartbeatResponse -> TestTree
reportTaskRunnerHeartbeatResponseTest = resp
    "reportTaskRunnerHeartbeatResponse"
    "fixture/ReportTaskRunnerHeartbeatResponse"
    (Proxy :: Proxy ReportTaskRunnerHeartbeat)

setStatusResponseTest :: SetStatusResponse -> TestTree
setStatusResponseTest = resp
    "setStatusResponse"
    "fixture/SetStatusResponse"
    (Proxy :: Proxy SetStatus)

setTaskStatusResponseTest :: SetTaskStatusResponse -> TestTree
setTaskStatusResponseTest = resp
    "setTaskStatusResponse"
    "fixture/SetTaskStatusResponse"
    (Proxy :: Proxy SetTaskStatus)

validatePipelineDefinitionResponseTest :: ValidatePipelineDefinitionResponse -> TestTree
validatePipelineDefinitionResponseTest = resp
    "validatePipelineDefinitionResponse"
    "fixture/ValidatePipelineDefinitionResponse"
    (Proxy :: Proxy ValidatePipelineDefinition)
