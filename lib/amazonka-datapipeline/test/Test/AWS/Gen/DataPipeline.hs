{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DataPipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.DataPipeline where

import Data.Proxy
import Network.AWS.DataPipeline
import Test.AWS.DataPipeline.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribePipelines $
--             describePipelines
--
--         , requestQueryObjects $
--             queryObjects
--
--         , requestRemoveTags $
--             removeTags
--
--         , requestDeletePipeline $
--             deletePipeline
--
--         , requestListPipelines $
--             listPipelines
--
--         , requestEvaluateExpression $
--             evaluateExpression
--
--         , requestGetPipelineDefinition $
--             getPipelineDefinition
--
--         , requestPollForTask $
--             pollForTask
--
--         , requestDeactivatePipeline $
--             deactivatePipeline
--
--         , requestAddTags $
--             addTags
--
--         , requestDescribeObjects $
--             describeObjects
--
--         , requestReportTaskRunnerHeartbeat $
--             reportTaskRunnerHeartbeat
--
--         , requestActivatePipeline $
--             activatePipeline
--
--         , requestSetTaskStatus $
--             setTaskStatus
--
--         , requestSetStatus $
--             setStatus
--
--         , requestReportTaskProgress $
--             reportTaskProgress
--
--         , requestCreatePipeline $
--             createPipeline
--
--         , requestPutPipelineDefinition $
--             putPipelineDefinition
--
--         , requestValidatePipelineDefinition $
--             validatePipelineDefinition
--
--           ]

--     , testGroup "response"
--         [ responseDescribePipelines $
--             describePipelinesResponse
--
--         , responseQueryObjects $
--             queryObjectsResponse
--
--         , responseRemoveTags $
--             removeTagsResponse
--
--         , responseDeletePipeline $
--             deletePipelineResponse
--
--         , responseListPipelines $
--             listPipelinesResponse
--
--         , responseEvaluateExpression $
--             evaluateExpressionResponse
--
--         , responseGetPipelineDefinition $
--             getPipelineDefinitionResponse
--
--         , responsePollForTask $
--             pollForTaskResponse
--
--         , responseDeactivatePipeline $
--             deactivatePipelineResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseDescribeObjects $
--             describeObjectsResponse
--
--         , responseReportTaskRunnerHeartbeat $
--             reportTaskRunnerHeartbeatResponse
--
--         , responseActivatePipeline $
--             activatePipelineResponse
--
--         , responseSetTaskStatus $
--             setTaskStatusResponse
--
--         , responseSetStatus $
--             setStatusResponse
--
--         , responseReportTaskProgress $
--             reportTaskProgressResponse
--
--         , responseCreatePipeline $
--             createPipelineResponse
--
--         , responsePutPipelineDefinition $
--             putPipelineDefinitionResponse
--
--         , responseValidatePipelineDefinition $
--             validatePipelineDefinitionResponse
--
--           ]
--     ]

-- Requests

requestDescribePipelines :: DescribePipelines -> TestTree
requestDescribePipelines = req
    "DescribePipelines"
    "fixture/DescribePipelines.yaml"

requestQueryObjects :: QueryObjects -> TestTree
requestQueryObjects = req
    "QueryObjects"
    "fixture/QueryObjects.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline = req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines = req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestEvaluateExpression :: EvaluateExpression -> TestTree
requestEvaluateExpression = req
    "EvaluateExpression"
    "fixture/EvaluateExpression.yaml"

requestGetPipelineDefinition :: GetPipelineDefinition -> TestTree
requestGetPipelineDefinition = req
    "GetPipelineDefinition"
    "fixture/GetPipelineDefinition.yaml"

requestPollForTask :: PollForTask -> TestTree
requestPollForTask = req
    "PollForTask"
    "fixture/PollForTask.yaml"

requestDeactivatePipeline :: DeactivatePipeline -> TestTree
requestDeactivatePipeline = req
    "DeactivatePipeline"
    "fixture/DeactivatePipeline.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

requestDescribeObjects :: DescribeObjects -> TestTree
requestDescribeObjects = req
    "DescribeObjects"
    "fixture/DescribeObjects.yaml"

requestReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeat -> TestTree
requestReportTaskRunnerHeartbeat = req
    "ReportTaskRunnerHeartbeat"
    "fixture/ReportTaskRunnerHeartbeat.yaml"

requestActivatePipeline :: ActivatePipeline -> TestTree
requestActivatePipeline = req
    "ActivatePipeline"
    "fixture/ActivatePipeline.yaml"

requestSetTaskStatus :: SetTaskStatus -> TestTree
requestSetTaskStatus = req
    "SetTaskStatus"
    "fixture/SetTaskStatus.yaml"

requestSetStatus :: SetStatus -> TestTree
requestSetStatus = req
    "SetStatus"
    "fixture/SetStatus.yaml"

requestReportTaskProgress :: ReportTaskProgress -> TestTree
requestReportTaskProgress = req
    "ReportTaskProgress"
    "fixture/ReportTaskProgress.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline = req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestPutPipelineDefinition :: PutPipelineDefinition -> TestTree
requestPutPipelineDefinition = req
    "PutPipelineDefinition"
    "fixture/PutPipelineDefinition.yaml"

requestValidatePipelineDefinition :: ValidatePipelineDefinition -> TestTree
requestValidatePipelineDefinition = req
    "ValidatePipelineDefinition"
    "fixture/ValidatePipelineDefinition.yaml"

-- Responses

responseDescribePipelines :: DescribePipelinesResponse -> TestTree
responseDescribePipelines = res
    "DescribePipelinesResponse"
    "fixture/DescribePipelinesResponse.proto"
    dataPipeline
    (Proxy :: Proxy DescribePipelines)

responseQueryObjects :: QueryObjectsResponse -> TestTree
responseQueryObjects = res
    "QueryObjectsResponse"
    "fixture/QueryObjectsResponse.proto"
    dataPipeline
    (Proxy :: Proxy QueryObjects)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    dataPipeline
    (Proxy :: Proxy RemoveTags)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline = res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    dataPipeline
    (Proxy :: Proxy DeletePipeline)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines = res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    dataPipeline
    (Proxy :: Proxy ListPipelines)

responseEvaluateExpression :: EvaluateExpressionResponse -> TestTree
responseEvaluateExpression = res
    "EvaluateExpressionResponse"
    "fixture/EvaluateExpressionResponse.proto"
    dataPipeline
    (Proxy :: Proxy EvaluateExpression)

responseGetPipelineDefinition :: GetPipelineDefinitionResponse -> TestTree
responseGetPipelineDefinition = res
    "GetPipelineDefinitionResponse"
    "fixture/GetPipelineDefinitionResponse.proto"
    dataPipeline
    (Proxy :: Proxy GetPipelineDefinition)

responsePollForTask :: PollForTaskResponse -> TestTree
responsePollForTask = res
    "PollForTaskResponse"
    "fixture/PollForTaskResponse.proto"
    dataPipeline
    (Proxy :: Proxy PollForTask)

responseDeactivatePipeline :: DeactivatePipelineResponse -> TestTree
responseDeactivatePipeline = res
    "DeactivatePipelineResponse"
    "fixture/DeactivatePipelineResponse.proto"
    dataPipeline
    (Proxy :: Proxy DeactivatePipeline)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    dataPipeline
    (Proxy :: Proxy AddTags)

responseDescribeObjects :: DescribeObjectsResponse -> TestTree
responseDescribeObjects = res
    "DescribeObjectsResponse"
    "fixture/DescribeObjectsResponse.proto"
    dataPipeline
    (Proxy :: Proxy DescribeObjects)

responseReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeatResponse -> TestTree
responseReportTaskRunnerHeartbeat = res
    "ReportTaskRunnerHeartbeatResponse"
    "fixture/ReportTaskRunnerHeartbeatResponse.proto"
    dataPipeline
    (Proxy :: Proxy ReportTaskRunnerHeartbeat)

responseActivatePipeline :: ActivatePipelineResponse -> TestTree
responseActivatePipeline = res
    "ActivatePipelineResponse"
    "fixture/ActivatePipelineResponse.proto"
    dataPipeline
    (Proxy :: Proxy ActivatePipeline)

responseSetTaskStatus :: SetTaskStatusResponse -> TestTree
responseSetTaskStatus = res
    "SetTaskStatusResponse"
    "fixture/SetTaskStatusResponse.proto"
    dataPipeline
    (Proxy :: Proxy SetTaskStatus)

responseSetStatus :: SetStatusResponse -> TestTree
responseSetStatus = res
    "SetStatusResponse"
    "fixture/SetStatusResponse.proto"
    dataPipeline
    (Proxy :: Proxy SetStatus)

responseReportTaskProgress :: ReportTaskProgressResponse -> TestTree
responseReportTaskProgress = res
    "ReportTaskProgressResponse"
    "fixture/ReportTaskProgressResponse.proto"
    dataPipeline
    (Proxy :: Proxy ReportTaskProgress)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline = res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    dataPipeline
    (Proxy :: Proxy CreatePipeline)

responsePutPipelineDefinition :: PutPipelineDefinitionResponse -> TestTree
responsePutPipelineDefinition = res
    "PutPipelineDefinitionResponse"
    "fixture/PutPipelineDefinitionResponse.proto"
    dataPipeline
    (Proxy :: Proxy PutPipelineDefinition)

responseValidatePipelineDefinition :: ValidatePipelineDefinitionResponse -> TestTree
responseValidatePipelineDefinition = res
    "ValidatePipelineDefinitionResponse"
    "fixture/ValidatePipelineDefinitionResponse.proto"
    dataPipeline
    (Proxy :: Proxy ValidatePipelineDefinition)
