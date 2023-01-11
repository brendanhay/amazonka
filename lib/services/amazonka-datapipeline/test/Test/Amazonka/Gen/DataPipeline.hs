{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DataPipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DataPipeline where

import Amazonka.DataPipeline
import qualified Data.Proxy as Proxy
import Test.Amazonka.DataPipeline.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestActivatePipeline $
--             newActivatePipeline
--
--         , requestAddTags $
--             newAddTags
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestDeactivatePipeline $
--             newDeactivatePipeline
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestDescribeObjects $
--             newDescribeObjects
--
--         , requestDescribePipelines $
--             newDescribePipelines
--
--         , requestEvaluateExpression $
--             newEvaluateExpression
--
--         , requestGetPipelineDefinition $
--             newGetPipelineDefinition
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestPollForTask $
--             newPollForTask
--
--         , requestPutPipelineDefinition $
--             newPutPipelineDefinition
--
--         , requestQueryObjects $
--             newQueryObjects
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestReportTaskProgress $
--             newReportTaskProgress
--
--         , requestReportTaskRunnerHeartbeat $
--             newReportTaskRunnerHeartbeat
--
--         , requestSetStatus $
--             newSetStatus
--
--         , requestSetTaskStatus $
--             newSetTaskStatus
--
--         , requestValidatePipelineDefinition $
--             newValidatePipelineDefinition
--
--           ]

--     , testGroup "response"
--         [ responseActivatePipeline $
--             newActivatePipelineResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseDeactivatePipeline $
--             newDeactivatePipelineResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseDescribeObjects $
--             newDescribeObjectsResponse
--
--         , responseDescribePipelines $
--             newDescribePipelinesResponse
--
--         , responseEvaluateExpression $
--             newEvaluateExpressionResponse
--
--         , responseGetPipelineDefinition $
--             newGetPipelineDefinitionResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responsePollForTask $
--             newPollForTaskResponse
--
--         , responsePutPipelineDefinition $
--             newPutPipelineDefinitionResponse
--
--         , responseQueryObjects $
--             newQueryObjectsResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseReportTaskProgress $
--             newReportTaskProgressResponse
--
--         , responseReportTaskRunnerHeartbeat $
--             newReportTaskRunnerHeartbeatResponse
--
--         , responseSetStatus $
--             newSetStatusResponse
--
--         , responseSetTaskStatus $
--             newSetTaskStatusResponse
--
--         , responseValidatePipelineDefinition $
--             newValidatePipelineDefinitionResponse
--
--           ]
--     ]

-- Requests

requestActivatePipeline :: ActivatePipeline -> TestTree
requestActivatePipeline =
  req
    "ActivatePipeline"
    "fixture/ActivatePipeline.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestDeactivatePipeline :: DeactivatePipeline -> TestTree
requestDeactivatePipeline =
  req
    "DeactivatePipeline"
    "fixture/DeactivatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestDescribeObjects :: DescribeObjects -> TestTree
requestDescribeObjects =
  req
    "DescribeObjects"
    "fixture/DescribeObjects.yaml"

requestDescribePipelines :: DescribePipelines -> TestTree
requestDescribePipelines =
  req
    "DescribePipelines"
    "fixture/DescribePipelines.yaml"

requestEvaluateExpression :: EvaluateExpression -> TestTree
requestEvaluateExpression =
  req
    "EvaluateExpression"
    "fixture/EvaluateExpression.yaml"

requestGetPipelineDefinition :: GetPipelineDefinition -> TestTree
requestGetPipelineDefinition =
  req
    "GetPipelineDefinition"
    "fixture/GetPipelineDefinition.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestPollForTask :: PollForTask -> TestTree
requestPollForTask =
  req
    "PollForTask"
    "fixture/PollForTask.yaml"

requestPutPipelineDefinition :: PutPipelineDefinition -> TestTree
requestPutPipelineDefinition =
  req
    "PutPipelineDefinition"
    "fixture/PutPipelineDefinition.yaml"

requestQueryObjects :: QueryObjects -> TestTree
requestQueryObjects =
  req
    "QueryObjects"
    "fixture/QueryObjects.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestReportTaskProgress :: ReportTaskProgress -> TestTree
requestReportTaskProgress =
  req
    "ReportTaskProgress"
    "fixture/ReportTaskProgress.yaml"

requestReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeat -> TestTree
requestReportTaskRunnerHeartbeat =
  req
    "ReportTaskRunnerHeartbeat"
    "fixture/ReportTaskRunnerHeartbeat.yaml"

requestSetStatus :: SetStatus -> TestTree
requestSetStatus =
  req
    "SetStatus"
    "fixture/SetStatus.yaml"

requestSetTaskStatus :: SetTaskStatus -> TestTree
requestSetTaskStatus =
  req
    "SetTaskStatus"
    "fixture/SetTaskStatus.yaml"

requestValidatePipelineDefinition :: ValidatePipelineDefinition -> TestTree
requestValidatePipelineDefinition =
  req
    "ValidatePipelineDefinition"
    "fixture/ValidatePipelineDefinition.yaml"

-- Responses

responseActivatePipeline :: ActivatePipelineResponse -> TestTree
responseActivatePipeline =
  res
    "ActivatePipelineResponse"
    "fixture/ActivatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivatePipeline)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePipeline)

responseDeactivatePipeline :: DeactivatePipelineResponse -> TestTree
responseDeactivatePipeline =
  res
    "DeactivatePipelineResponse"
    "fixture/DeactivatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePipeline)

responseDescribeObjects :: DescribeObjectsResponse -> TestTree
responseDescribeObjects =
  res
    "DescribeObjectsResponse"
    "fixture/DescribeObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeObjects)

responseDescribePipelines :: DescribePipelinesResponse -> TestTree
responseDescribePipelines =
  res
    "DescribePipelinesResponse"
    "fixture/DescribePipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePipelines)

responseEvaluateExpression :: EvaluateExpressionResponse -> TestTree
responseEvaluateExpression =
  res
    "EvaluateExpressionResponse"
    "fixture/EvaluateExpressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EvaluateExpression)

responseGetPipelineDefinition :: GetPipelineDefinitionResponse -> TestTree
responseGetPipelineDefinition =
  res
    "GetPipelineDefinitionResponse"
    "fixture/GetPipelineDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPipelineDefinition)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelines)

responsePollForTask :: PollForTaskResponse -> TestTree
responsePollForTask =
  res
    "PollForTaskResponse"
    "fixture/PollForTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PollForTask)

responsePutPipelineDefinition :: PutPipelineDefinitionResponse -> TestTree
responsePutPipelineDefinition =
  res
    "PutPipelineDefinitionResponse"
    "fixture/PutPipelineDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPipelineDefinition)

responseQueryObjects :: QueryObjectsResponse -> TestTree
responseQueryObjects =
  res
    "QueryObjectsResponse"
    "fixture/QueryObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy QueryObjects)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTags)

responseReportTaskProgress :: ReportTaskProgressResponse -> TestTree
responseReportTaskProgress =
  res
    "ReportTaskProgressResponse"
    "fixture/ReportTaskProgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReportTaskProgress)

responseReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeatResponse -> TestTree
responseReportTaskRunnerHeartbeat =
  res
    "ReportTaskRunnerHeartbeatResponse"
    "fixture/ReportTaskRunnerHeartbeatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReportTaskRunnerHeartbeat)

responseSetStatus :: SetStatusResponse -> TestTree
responseSetStatus =
  res
    "SetStatusResponse"
    "fixture/SetStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetStatus)

responseSetTaskStatus :: SetTaskStatusResponse -> TestTree
responseSetTaskStatus =
  res
    "SetTaskStatusResponse"
    "fixture/SetTaskStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTaskStatus)

responseValidatePipelineDefinition :: ValidatePipelineDefinitionResponse -> TestTree
responseValidatePipelineDefinition =
  res
    "ValidatePipelineDefinitionResponse"
    "fixture/ValidatePipelineDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidatePipelineDefinition)
