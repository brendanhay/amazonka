{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DataPipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestDescribePipelines $
--             mkDescribePipelines
--
--         , requestQueryObjects $
--             mkQueryObjects
--
--         , requestRemoveTags $
--             mkRemoveTags
--
--         , requestDeletePipeline $
--             mkDeletePipeline
--
--         , requestListPipelines $
--             mkListPipelines
--
--         , requestEvaluateExpression $
--             mkEvaluateExpression
--
--         , requestGetPipelineDefinition $
--             mkGetPipelineDefinition
--
--         , requestPollForTask $
--             mkPollForTask
--
--         , requestDeactivatePipeline $
--             mkDeactivatePipeline
--
--         , requestAddTags $
--             mkAddTags
--
--         , requestDescribeObjects $
--             mkDescribeObjects
--
--         , requestReportTaskRunnerHeartbeat $
--             mkReportTaskRunnerHeartbeat
--
--         , requestActivatePipeline $
--             mkActivatePipeline
--
--         , requestSetTaskStatus $
--             mkSetTaskStatus
--
--         , requestSetStatus $
--             mkSetStatus
--
--         , requestReportTaskProgress $
--             mkReportTaskProgress
--
--         , requestCreatePipeline $
--             mkCreatePipeline
--
--         , requestPutPipelineDefinition $
--             mkPutPipelineDefinition
--
--         , requestValidatePipelineDefinition $
--             mkValidatePipelineDefinition
--
--           ]

--     , testGroup "response"
--         [ responseDescribePipelines $
--             mkDescribePipelinesResponse
--
--         , responseQueryObjects $
--             mkQueryObjectsResponse
--
--         , responseRemoveTags $
--             mkRemoveTagsResponse
--
--         , responseDeletePipeline $
--             mkDeletePipelineResponse
--
--         , responseListPipelines $
--             mkListPipelinesResponse
--
--         , responseEvaluateExpression $
--             mkEvaluateExpressionResponse
--
--         , responseGetPipelineDefinition $
--             mkGetPipelineDefinitionResponse
--
--         , responsePollForTask $
--             mkPollForTaskResponse
--
--         , responseDeactivatePipeline $
--             mkDeactivatePipelineResponse
--
--         , responseAddTags $
--             mkAddTagsResponse
--
--         , responseDescribeObjects $
--             mkDescribeObjectsResponse
--
--         , responseReportTaskRunnerHeartbeat $
--             mkReportTaskRunnerHeartbeatResponse
--
--         , responseActivatePipeline $
--             mkActivatePipelineResponse
--
--         , responseSetTaskStatus $
--             mkSetTaskStatusResponse
--
--         , responseSetStatus $
--             mkSetStatusResponse
--
--         , responseReportTaskProgress $
--             mkReportTaskProgressResponse
--
--         , responseCreatePipeline $
--             mkCreatePipelineResponse
--
--         , responsePutPipelineDefinition $
--             mkPutPipelineDefinitionResponse
--
--         , responseValidatePipelineDefinition $
--             mkValidatePipelineDefinitionResponse
--
--           ]
--     ]

-- Requests

requestDescribePipelines :: DescribePipelines -> TestTree
requestDescribePipelines =
  req
    "DescribePipelines"
    "fixture/DescribePipelines.yaml"

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

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

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

requestPollForTask :: PollForTask -> TestTree
requestPollForTask =
  req
    "PollForTask"
    "fixture/PollForTask.yaml"

requestDeactivatePipeline :: DeactivatePipeline -> TestTree
requestDeactivatePipeline =
  req
    "DeactivatePipeline"
    "fixture/DeactivatePipeline.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestDescribeObjects :: DescribeObjects -> TestTree
requestDescribeObjects =
  req
    "DescribeObjects"
    "fixture/DescribeObjects.yaml"

requestReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeat -> TestTree
requestReportTaskRunnerHeartbeat =
  req
    "ReportTaskRunnerHeartbeat"
    "fixture/ReportTaskRunnerHeartbeat.yaml"

requestActivatePipeline :: ActivatePipeline -> TestTree
requestActivatePipeline =
  req
    "ActivatePipeline"
    "fixture/ActivatePipeline.yaml"

requestSetTaskStatus :: SetTaskStatus -> TestTree
requestSetTaskStatus =
  req
    "SetTaskStatus"
    "fixture/SetTaskStatus.yaml"

requestSetStatus :: SetStatus -> TestTree
requestSetStatus =
  req
    "SetStatus"
    "fixture/SetStatus.yaml"

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

requestPutPipelineDefinition :: PutPipelineDefinition -> TestTree
requestPutPipelineDefinition =
  req
    "PutPipelineDefinition"
    "fixture/PutPipelineDefinition.yaml"

requestValidatePipelineDefinition :: ValidatePipelineDefinition -> TestTree
requestValidatePipelineDefinition =
  req
    "ValidatePipelineDefinition"
    "fixture/ValidatePipelineDefinition.yaml"

-- Responses

responseDescribePipelines :: DescribePipelinesResponse -> TestTree
responseDescribePipelines =
  res
    "DescribePipelinesResponse"
    "fixture/DescribePipelinesResponse.proto"
    dataPipelineService
    (Proxy :: Proxy DescribePipelines)

responseQueryObjects :: QueryObjectsResponse -> TestTree
responseQueryObjects =
  res
    "QueryObjectsResponse"
    "fixture/QueryObjectsResponse.proto"
    dataPipelineService
    (Proxy :: Proxy QueryObjects)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    dataPipelineService
    (Proxy :: Proxy RemoveTags)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    dataPipelineService
    (Proxy :: Proxy DeletePipeline)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    dataPipelineService
    (Proxy :: Proxy ListPipelines)

responseEvaluateExpression :: EvaluateExpressionResponse -> TestTree
responseEvaluateExpression =
  res
    "EvaluateExpressionResponse"
    "fixture/EvaluateExpressionResponse.proto"
    dataPipelineService
    (Proxy :: Proxy EvaluateExpression)

responseGetPipelineDefinition :: GetPipelineDefinitionResponse -> TestTree
responseGetPipelineDefinition =
  res
    "GetPipelineDefinitionResponse"
    "fixture/GetPipelineDefinitionResponse.proto"
    dataPipelineService
    (Proxy :: Proxy GetPipelineDefinition)

responsePollForTask :: PollForTaskResponse -> TestTree
responsePollForTask =
  res
    "PollForTaskResponse"
    "fixture/PollForTaskResponse.proto"
    dataPipelineService
    (Proxy :: Proxy PollForTask)

responseDeactivatePipeline :: DeactivatePipelineResponse -> TestTree
responseDeactivatePipeline =
  res
    "DeactivatePipelineResponse"
    "fixture/DeactivatePipelineResponse.proto"
    dataPipelineService
    (Proxy :: Proxy DeactivatePipeline)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    dataPipelineService
    (Proxy :: Proxy AddTags)

responseDescribeObjects :: DescribeObjectsResponse -> TestTree
responseDescribeObjects =
  res
    "DescribeObjectsResponse"
    "fixture/DescribeObjectsResponse.proto"
    dataPipelineService
    (Proxy :: Proxy DescribeObjects)

responseReportTaskRunnerHeartbeat :: ReportTaskRunnerHeartbeatResponse -> TestTree
responseReportTaskRunnerHeartbeat =
  res
    "ReportTaskRunnerHeartbeatResponse"
    "fixture/ReportTaskRunnerHeartbeatResponse.proto"
    dataPipelineService
    (Proxy :: Proxy ReportTaskRunnerHeartbeat)

responseActivatePipeline :: ActivatePipelineResponse -> TestTree
responseActivatePipeline =
  res
    "ActivatePipelineResponse"
    "fixture/ActivatePipelineResponse.proto"
    dataPipelineService
    (Proxy :: Proxy ActivatePipeline)

responseSetTaskStatus :: SetTaskStatusResponse -> TestTree
responseSetTaskStatus =
  res
    "SetTaskStatusResponse"
    "fixture/SetTaskStatusResponse.proto"
    dataPipelineService
    (Proxy :: Proxy SetTaskStatus)

responseSetStatus :: SetStatusResponse -> TestTree
responseSetStatus =
  res
    "SetStatusResponse"
    "fixture/SetStatusResponse.proto"
    dataPipelineService
    (Proxy :: Proxy SetStatus)

responseReportTaskProgress :: ReportTaskProgressResponse -> TestTree
responseReportTaskProgress =
  res
    "ReportTaskProgressResponse"
    "fixture/ReportTaskProgressResponse.proto"
    dataPipelineService
    (Proxy :: Proxy ReportTaskProgress)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    dataPipelineService
    (Proxy :: Proxy CreatePipeline)

responsePutPipelineDefinition :: PutPipelineDefinitionResponse -> TestTree
responsePutPipelineDefinition =
  res
    "PutPipelineDefinitionResponse"
    "fixture/PutPipelineDefinitionResponse.proto"
    dataPipelineService
    (Proxy :: Proxy PutPipelineDefinition)

responseValidatePipelineDefinition :: ValidatePipelineDefinitionResponse -> TestTree
responseValidatePipelineDefinition =
  res
    "ValidatePipelineDefinitionResponse"
    "fixture/ValidatePipelineDefinitionResponse.proto"
    dataPipelineService
    (Proxy :: Proxy ValidatePipelineDefinition)
