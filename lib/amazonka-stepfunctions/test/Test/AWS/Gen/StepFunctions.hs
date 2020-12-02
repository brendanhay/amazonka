{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.StepFunctions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.StepFunctions where

import Data.Proxy
import Network.AWS.StepFunctions
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.StepFunctions.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteActivity $
--             deleteActivity
--
--         , requestDescribeStateMachine $
--             describeStateMachine
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestStopExecution $
--             stopExecution
--
--         , requestDescribeActivity $
--             describeActivity
--
--         , requestListStateMachines $
--             listStateMachines
--
--         , requestListExecutions $
--             listExecutions
--
--         , requestDeleteStateMachine $
--             deleteStateMachine
--
--         , requestUpdateStateMachine $
--             updateStateMachine
--
--         , requestDescribeStateMachineForExecution $
--             describeStateMachineForExecution
--
--         , requestGetActivityTask $
--             getActivityTask
--
--         , requestCreateActivity $
--             createActivity
--
--         , requestListActivities $
--             listActivities
--
--         , requestSendTaskHeartbeat $
--             sendTaskHeartbeat
--
--         , requestSendTaskFailure $
--             sendTaskFailure
--
--         , requestDescribeExecution $
--             describeExecution
--
--         , requestSendTaskSuccess $
--             sendTaskSuccess
--
--         , requestStartExecution $
--             startExecution
--
--         , requestStartSyncExecution $
--             startSyncExecution
--
--         , requestTagResource $
--             tagResource
--
--         , requestUntagResource $
--             untagResource
--
--         , requestGetExecutionHistory $
--             getExecutionHistory
--
--         , requestCreateStateMachine $
--             createStateMachine
--
--           ]

--     , testGroup "response"
--         [ responseDeleteActivity $
--             deleteActivityResponse
--
--         , responseDescribeStateMachine $
--             describeStateMachineResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseStopExecution $
--             stopExecutionResponse
--
--         , responseDescribeActivity $
--             describeActivityResponse
--
--         , responseListStateMachines $
--             listStateMachinesResponse
--
--         , responseListExecutions $
--             listExecutionsResponse
--
--         , responseDeleteStateMachine $
--             deleteStateMachineResponse
--
--         , responseUpdateStateMachine $
--             updateStateMachineResponse
--
--         , responseDescribeStateMachineForExecution $
--             describeStateMachineForExecutionResponse
--
--         , responseGetActivityTask $
--             getActivityTaskResponse
--
--         , responseCreateActivity $
--             createActivityResponse
--
--         , responseListActivities $
--             listActivitiesResponse
--
--         , responseSendTaskHeartbeat $
--             sendTaskHeartbeatResponse
--
--         , responseSendTaskFailure $
--             sendTaskFailureResponse
--
--         , responseDescribeExecution $
--             describeExecutionResponse
--
--         , responseSendTaskSuccess $
--             sendTaskSuccessResponse
--
--         , responseStartExecution $
--             startExecutionResponse
--
--         , responseStartSyncExecution $
--             startSyncExecutionResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseGetExecutionHistory $
--             getExecutionHistoryResponse
--
--         , responseCreateStateMachine $
--             createStateMachineResponse
--
--           ]
--     ]

-- Requests

requestDeleteActivity :: DeleteActivity -> TestTree
requestDeleteActivity =
  req
    "DeleteActivity"
    "fixture/DeleteActivity.yaml"

requestDescribeStateMachine :: DescribeStateMachine -> TestTree
requestDescribeStateMachine =
  req
    "DescribeStateMachine"
    "fixture/DescribeStateMachine.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStopExecution :: StopExecution -> TestTree
requestStopExecution =
  req
    "StopExecution"
    "fixture/StopExecution.yaml"

requestDescribeActivity :: DescribeActivity -> TestTree
requestDescribeActivity =
  req
    "DescribeActivity"
    "fixture/DescribeActivity.yaml"

requestListStateMachines :: ListStateMachines -> TestTree
requestListStateMachines =
  req
    "ListStateMachines"
    "fixture/ListStateMachines.yaml"

requestListExecutions :: ListExecutions -> TestTree
requestListExecutions =
  req
    "ListExecutions"
    "fixture/ListExecutions.yaml"

requestDeleteStateMachine :: DeleteStateMachine -> TestTree
requestDeleteStateMachine =
  req
    "DeleteStateMachine"
    "fixture/DeleteStateMachine.yaml"

requestUpdateStateMachine :: UpdateStateMachine -> TestTree
requestUpdateStateMachine =
  req
    "UpdateStateMachine"
    "fixture/UpdateStateMachine.yaml"

requestDescribeStateMachineForExecution :: DescribeStateMachineForExecution -> TestTree
requestDescribeStateMachineForExecution =
  req
    "DescribeStateMachineForExecution"
    "fixture/DescribeStateMachineForExecution.yaml"

requestGetActivityTask :: GetActivityTask -> TestTree
requestGetActivityTask =
  req
    "GetActivityTask"
    "fixture/GetActivityTask.yaml"

requestCreateActivity :: CreateActivity -> TestTree
requestCreateActivity =
  req
    "CreateActivity"
    "fixture/CreateActivity.yaml"

requestListActivities :: ListActivities -> TestTree
requestListActivities =
  req
    "ListActivities"
    "fixture/ListActivities.yaml"

requestSendTaskHeartbeat :: SendTaskHeartbeat -> TestTree
requestSendTaskHeartbeat =
  req
    "SendTaskHeartbeat"
    "fixture/SendTaskHeartbeat.yaml"

requestSendTaskFailure :: SendTaskFailure -> TestTree
requestSendTaskFailure =
  req
    "SendTaskFailure"
    "fixture/SendTaskFailure.yaml"

requestDescribeExecution :: DescribeExecution -> TestTree
requestDescribeExecution =
  req
    "DescribeExecution"
    "fixture/DescribeExecution.yaml"

requestSendTaskSuccess :: SendTaskSuccess -> TestTree
requestSendTaskSuccess =
  req
    "SendTaskSuccess"
    "fixture/SendTaskSuccess.yaml"

requestStartExecution :: StartExecution -> TestTree
requestStartExecution =
  req
    "StartExecution"
    "fixture/StartExecution.yaml"

requestStartSyncExecution :: StartSyncExecution -> TestTree
requestStartSyncExecution =
  req
    "StartSyncExecution"
    "fixture/StartSyncExecution.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetExecutionHistory :: GetExecutionHistory -> TestTree
requestGetExecutionHistory =
  req
    "GetExecutionHistory"
    "fixture/GetExecutionHistory.yaml"

requestCreateStateMachine :: CreateStateMachine -> TestTree
requestCreateStateMachine =
  req
    "CreateStateMachine"
    "fixture/CreateStateMachine.yaml"

-- Responses

responseDeleteActivity :: DeleteActivityResponse -> TestTree
responseDeleteActivity =
  res
    "DeleteActivityResponse"
    "fixture/DeleteActivityResponse.proto"
    stepFunctions
    (Proxy :: Proxy DeleteActivity)

responseDescribeStateMachine :: DescribeStateMachineResponse -> TestTree
responseDescribeStateMachine =
  res
    "DescribeStateMachineResponse"
    "fixture/DescribeStateMachineResponse.proto"
    stepFunctions
    (Proxy :: Proxy DescribeStateMachine)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    stepFunctions
    (Proxy :: Proxy ListTagsForResource)

responseStopExecution :: StopExecutionResponse -> TestTree
responseStopExecution =
  res
    "StopExecutionResponse"
    "fixture/StopExecutionResponse.proto"
    stepFunctions
    (Proxy :: Proxy StopExecution)

responseDescribeActivity :: DescribeActivityResponse -> TestTree
responseDescribeActivity =
  res
    "DescribeActivityResponse"
    "fixture/DescribeActivityResponse.proto"
    stepFunctions
    (Proxy :: Proxy DescribeActivity)

responseListStateMachines :: ListStateMachinesResponse -> TestTree
responseListStateMachines =
  res
    "ListStateMachinesResponse"
    "fixture/ListStateMachinesResponse.proto"
    stepFunctions
    (Proxy :: Proxy ListStateMachines)

responseListExecutions :: ListExecutionsResponse -> TestTree
responseListExecutions =
  res
    "ListExecutionsResponse"
    "fixture/ListExecutionsResponse.proto"
    stepFunctions
    (Proxy :: Proxy ListExecutions)

responseDeleteStateMachine :: DeleteStateMachineResponse -> TestTree
responseDeleteStateMachine =
  res
    "DeleteStateMachineResponse"
    "fixture/DeleteStateMachineResponse.proto"
    stepFunctions
    (Proxy :: Proxy DeleteStateMachine)

responseUpdateStateMachine :: UpdateStateMachineResponse -> TestTree
responseUpdateStateMachine =
  res
    "UpdateStateMachineResponse"
    "fixture/UpdateStateMachineResponse.proto"
    stepFunctions
    (Proxy :: Proxy UpdateStateMachine)

responseDescribeStateMachineForExecution :: DescribeStateMachineForExecutionResponse -> TestTree
responseDescribeStateMachineForExecution =
  res
    "DescribeStateMachineForExecutionResponse"
    "fixture/DescribeStateMachineForExecutionResponse.proto"
    stepFunctions
    (Proxy :: Proxy DescribeStateMachineForExecution)

responseGetActivityTask :: GetActivityTaskResponse -> TestTree
responseGetActivityTask =
  res
    "GetActivityTaskResponse"
    "fixture/GetActivityTaskResponse.proto"
    stepFunctions
    (Proxy :: Proxy GetActivityTask)

responseCreateActivity :: CreateActivityResponse -> TestTree
responseCreateActivity =
  res
    "CreateActivityResponse"
    "fixture/CreateActivityResponse.proto"
    stepFunctions
    (Proxy :: Proxy CreateActivity)

responseListActivities :: ListActivitiesResponse -> TestTree
responseListActivities =
  res
    "ListActivitiesResponse"
    "fixture/ListActivitiesResponse.proto"
    stepFunctions
    (Proxy :: Proxy ListActivities)

responseSendTaskHeartbeat :: SendTaskHeartbeatResponse -> TestTree
responseSendTaskHeartbeat =
  res
    "SendTaskHeartbeatResponse"
    "fixture/SendTaskHeartbeatResponse.proto"
    stepFunctions
    (Proxy :: Proxy SendTaskHeartbeat)

responseSendTaskFailure :: SendTaskFailureResponse -> TestTree
responseSendTaskFailure =
  res
    "SendTaskFailureResponse"
    "fixture/SendTaskFailureResponse.proto"
    stepFunctions
    (Proxy :: Proxy SendTaskFailure)

responseDescribeExecution :: DescribeExecutionResponse -> TestTree
responseDescribeExecution =
  res
    "DescribeExecutionResponse"
    "fixture/DescribeExecutionResponse.proto"
    stepFunctions
    (Proxy :: Proxy DescribeExecution)

responseSendTaskSuccess :: SendTaskSuccessResponse -> TestTree
responseSendTaskSuccess =
  res
    "SendTaskSuccessResponse"
    "fixture/SendTaskSuccessResponse.proto"
    stepFunctions
    (Proxy :: Proxy SendTaskSuccess)

responseStartExecution :: StartExecutionResponse -> TestTree
responseStartExecution =
  res
    "StartExecutionResponse"
    "fixture/StartExecutionResponse.proto"
    stepFunctions
    (Proxy :: Proxy StartExecution)

responseStartSyncExecution :: StartSyncExecutionResponse -> TestTree
responseStartSyncExecution =
  res
    "StartSyncExecutionResponse"
    "fixture/StartSyncExecutionResponse.proto"
    stepFunctions
    (Proxy :: Proxy StartSyncExecution)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    stepFunctions
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    stepFunctions
    (Proxy :: Proxy UntagResource)

responseGetExecutionHistory :: GetExecutionHistoryResponse -> TestTree
responseGetExecutionHistory =
  res
    "GetExecutionHistoryResponse"
    "fixture/GetExecutionHistoryResponse.proto"
    stepFunctions
    (Proxy :: Proxy GetExecutionHistory)

responseCreateStateMachine :: CreateStateMachineResponse -> TestTree
responseCreateStateMachine =
  res
    "CreateStateMachineResponse"
    "fixture/CreateStateMachineResponse.proto"
    stepFunctions
    (Proxy :: Proxy CreateStateMachine)
