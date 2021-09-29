{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DataPipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestRemoveTags $
--             newRemoveTags
--
--         , requestQueryObjects $
--             newQueryObjects
--
--         , requestEvaluateExpression $
--             newEvaluateExpression
--
--         , requestReportTaskProgress $
--             newReportTaskProgress
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestSetTaskStatus $
--             newSetTaskStatus
--
--         , requestReportTaskRunnerHeartbeat $
--             newReportTaskRunnerHeartbeat
--
--         , requestDescribeObjects $
--             newDescribeObjects
--
--         , requestAddTags $
--             newAddTags
--
--         , requestDescribePipelines $
--             newDescribePipelines
--
--         , requestPollForTask $
--             newPollForTask
--
--         , requestValidatePipelineDefinition $
--             newValidatePipelineDefinition
--
--         , requestGetPipelineDefinition $
--             newGetPipelineDefinition
--
--         , requestPutPipelineDefinition $
--             newPutPipelineDefinition
--
--         , requestSetStatus $
--             newSetStatus
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestActivatePipeline $
--             newActivatePipeline
--
--         , requestDeactivatePipeline $
--             newDeactivatePipeline
--
--           ]

--     , testGroup "response"
--         [ responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseQueryObjects $
--             newQueryObjectsResponse
--
--         , responseEvaluateExpression $
--             newEvaluateExpressionResponse
--
--         , responseReportTaskProgress $
--             newReportTaskProgressResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseSetTaskStatus $
--             newSetTaskStatusResponse
--
--         , responseReportTaskRunnerHeartbeat $
--             newReportTaskRunnerHeartbeatResponse
--
--         , responseDescribeObjects $
--             newDescribeObjectsResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseDescribePipelines $
--             newDescribePipelinesResponse
--
--         , responsePollForTask $
--             newPollForTaskResponse
--
--         , responseValidatePipelineDefinition $
--             newValidatePipelineDefinitionResponse
--
--         , responseGetPipelineDefinition $
--             newGetPipelineDefinitionResponse
--
--         , responsePutPipelineDefinition $
--             newPutPipelineDefinitionResponse
--
--         , responseSetStatus $
--             newSetStatusResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseActivatePipeline $
--             newActivatePipelineResponse
--
--         , responseDeactivatePipeline $
--             newDeactivatePipelineResponse
--
--           ]
--     ]

-- Requests

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestQueryObjects :: QueryObjects -> TestTree
requestQueryObjects =
  req
    "QueryObjects"
    "fixture/QueryObjects.yaml"

requestEvaluateExpression :: EvaluateExpression -> TestTree
requestEvaluateExpression =
  req
    "EvaluateExpression"
    "fixture/EvaluateExpression.yaml"

requestReportTaskProgress :: ReportTaskProgress -> TestTree
requestReportTaskProgress =
  req
    "ReportTaskProgress"
    "fixture/ReportTaskProgress.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestSetTaskStatus :: SetTaskStatus -> TestTree
requestSetTaskStatus =
  req
    "SetTaskStatus"
    "fixture/SetTaskStatus.yaml"

requestReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeat -> TestTree
requestReportTaskRunnerHeartbeat =
  req
    "ReportTaskRunnerHeartbeat"
    "fixture/ReportTaskRunnerHeartbeat.yaml"

requestDescribeObjects :: DescribeObjects -> TestTree
requestDescribeObjects =
  req
    "DescribeObjects"
    "fixture/DescribeObjects.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestDescribePipelines :: DescribePipelines -> TestTree
requestDescribePipelines =
  req
    "DescribePipelines"
    "fixture/DescribePipelines.yaml"

requestPollForTask :: PollForTask -> TestTree
requestPollForTask =
  req
    "PollForTask"
    "fixture/PollForTask.yaml"

requestValidatePipelineDefinition :: ValidatePipelineDefinition -> TestTree
requestValidatePipelineDefinition =
  req
    "ValidatePipelineDefinition"
    "fixture/ValidatePipelineDefinition.yaml"

requestGetPipelineDefinition :: GetPipelineDefinition -> TestTree
requestGetPipelineDefinition =
  req
    "GetPipelineDefinition"
    "fixture/GetPipelineDefinition.yaml"

requestPutPipelineDefinition :: PutPipelineDefinition -> TestTree
requestPutPipelineDefinition =
  req
    "PutPipelineDefinition"
    "fixture/PutPipelineDefinition.yaml"

requestSetStatus :: SetStatus -> TestTree
requestSetStatus =
  req
    "SetStatus"
    "fixture/SetStatus.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestActivatePipeline :: ActivatePipeline -> TestTree
requestActivatePipeline =
  req
    "ActivatePipeline"
    "fixture/ActivatePipeline.yaml"

requestDeactivatePipeline :: DeactivatePipeline -> TestTree
requestDeactivatePipeline =
  req
    "DeactivatePipeline"
    "fixture/DeactivatePipeline.yaml"

-- Responses

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTags)

responseQueryObjects :: QueryObjectsResponse -> TestTree
responseQueryObjects =
  res
    "QueryObjectsResponse"
    "fixture/QueryObjectsResponse.proto"
    defaultService
    (Proxy :: Proxy QueryObjects)

responseEvaluateExpression :: EvaluateExpressionResponse -> TestTree
responseEvaluateExpression =
  res
    "EvaluateExpressionResponse"
    "fixture/EvaluateExpressionResponse.proto"
    defaultService
    (Proxy :: Proxy EvaluateExpression)

responseReportTaskProgress :: ReportTaskProgressResponse -> TestTree
responseReportTaskProgress =
  res
    "ReportTaskProgressResponse"
    "fixture/ReportTaskProgressResponse.proto"
    defaultService
    (Proxy :: Proxy ReportTaskProgress)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePipeline)

responseSetTaskStatus :: SetTaskStatusResponse -> TestTree
responseSetTaskStatus =
  res
    "SetTaskStatusResponse"
    "fixture/SetTaskStatusResponse.proto"
    defaultService
    (Proxy :: Proxy SetTaskStatus)

responseReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeatResponse -> TestTree
responseReportTaskRunnerHeartbeat =
  res
    "ReportTaskRunnerHeartbeatResponse"
    "fixture/ReportTaskRunnerHeartbeatResponse.proto"
    defaultService
    (Proxy :: Proxy ReportTaskRunnerHeartbeat)

responseDescribeObjects :: DescribeObjectsResponse -> TestTree
responseDescribeObjects =
  res
    "DescribeObjectsResponse"
    "fixture/DescribeObjectsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeObjects)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy :: Proxy AddTags)

responseDescribePipelines :: DescribePipelinesResponse -> TestTree
responseDescribePipelines =
  res
    "DescribePipelinesResponse"
    "fixture/DescribePipelinesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePipelines)

responsePollForTask :: PollForTaskResponse -> TestTree
responsePollForTask =
  res
    "PollForTaskResponse"
    "fixture/PollForTaskResponse.proto"
    defaultService
    (Proxy :: Proxy PollForTask)

responseValidatePipelineDefinition :: ValidatePipelineDefinitionResponse -> TestTree
responseValidatePipelineDefinition =
  res
    "ValidatePipelineDefinitionResponse"
    "fixture/ValidatePipelineDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy ValidatePipelineDefinition)

responseGetPipelineDefinition :: GetPipelineDefinitionResponse -> TestTree
responseGetPipelineDefinition =
  res
    "GetPipelineDefinitionResponse"
    "fixture/GetPipelineDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetPipelineDefinition)

responsePutPipelineDefinition :: PutPipelineDefinitionResponse -> TestTree
responsePutPipelineDefinition =
  res
    "PutPipelineDefinitionResponse"
    "fixture/PutPipelineDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy PutPipelineDefinition)

responseSetStatus :: SetStatusResponse -> TestTree
responseSetStatus =
  res
    "SetStatusResponse"
    "fixture/SetStatusResponse.proto"
    defaultService
    (Proxy :: Proxy SetStatus)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelines)

responseActivatePipeline :: ActivatePipelineResponse -> TestTree
responseActivatePipeline =
  res
    "ActivatePipelineResponse"
    "fixture/ActivatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy ActivatePipeline)

responseDeactivatePipeline :: DeactivatePipelineResponse -> TestTree
responseDeactivatePipeline =
  res
    "DeactivatePipelineResponse"
    "fixture/DeactivatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeactivatePipeline)
