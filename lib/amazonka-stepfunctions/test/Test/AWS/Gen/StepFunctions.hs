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
--             mkDeleteActivity
--
--         , requestDescribeStateMachine $
--             mkDescribeStateMachine
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestStopExecution $
--             mkStopExecution
--
--         , requestDescribeActivity $
--             mkDescribeActivity
--
--         , requestListStateMachines $
--             mkListStateMachines
--
--         , requestListExecutions $
--             mkListExecutions
--
--         , requestDeleteStateMachine $
--             mkDeleteStateMachine
--
--         , requestUpdateStateMachine $
--             mkUpdateStateMachine
--
--         , requestDescribeStateMachineForExecution $
--             mkDescribeStateMachineForExecution
--
--         , requestGetActivityTask $
--             mkGetActivityTask
--
--         , requestCreateActivity $
--             mkCreateActivity
--
--         , requestListActivities $
--             mkListActivities
--
--         , requestSendTaskHeartbeat $
--             mkSendTaskHeartbeat
--
--         , requestSendTaskFailure $
--             mkSendTaskFailure
--
--         , requestDescribeExecution $
--             mkDescribeExecution
--
--         , requestSendTaskSuccess $
--             mkSendTaskSuccess
--
--         , requestStartExecution $
--             mkStartExecution
--
--         , requestStartSyncExecution $
--             mkStartSyncExecution
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestGetExecutionHistory $
--             mkGetExecutionHistory
--
--         , requestCreateStateMachine $
--             mkCreateStateMachine
--
--           ]

--     , testGroup "response"
--         [ responseDeleteActivity $
--             mkDeleteActivityResponse
--
--         , responseDescribeStateMachine $
--             mkDescribeStateMachineResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseStopExecution $
--             mkStopExecutionResponse
--
--         , responseDescribeActivity $
--             mkDescribeActivityResponse
--
--         , responseListStateMachines $
--             mkListStateMachinesResponse
--
--         , responseListExecutions $
--             mkListExecutionsResponse
--
--         , responseDeleteStateMachine $
--             mkDeleteStateMachineResponse
--
--         , responseUpdateStateMachine $
--             mkUpdateStateMachineResponse
--
--         , responseDescribeStateMachineForExecution $
--             mkDescribeStateMachineForExecutionResponse
--
--         , responseGetActivityTask $
--             mkGetActivityTaskResponse
--
--         , responseCreateActivity $
--             mkCreateActivityResponse
--
--         , responseListActivities $
--             mkListActivitiesResponse
--
--         , responseSendTaskHeartbeat $
--             mkSendTaskHeartbeatResponse
--
--         , responseSendTaskFailure $
--             mkSendTaskFailureResponse
--
--         , responseDescribeExecution $
--             mkDescribeExecutionResponse
--
--         , responseSendTaskSuccess $
--             mkSendTaskSuccessResponse
--
--         , responseStartExecution $
--             mkStartExecutionResponse
--
--         , responseStartSyncExecution $
--             mkStartSyncExecutionResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseGetExecutionHistory $
--             mkGetExecutionHistoryResponse
--
--         , responseCreateStateMachine $
--             mkCreateStateMachineResponse
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
    mkServiceConfig
    (Proxy :: Proxy DeleteActivity)

responseDescribeStateMachine :: DescribeStateMachineResponse -> TestTree
responseDescribeStateMachine =
  res
    "DescribeStateMachineResponse"
    "fixture/DescribeStateMachineResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStateMachine)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseStopExecution :: StopExecutionResponse -> TestTree
responseStopExecution =
  res
    "StopExecutionResponse"
    "fixture/StopExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopExecution)

responseDescribeActivity :: DescribeActivityResponse -> TestTree
responseDescribeActivity =
  res
    "DescribeActivityResponse"
    "fixture/DescribeActivityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeActivity)

responseListStateMachines :: ListStateMachinesResponse -> TestTree
responseListStateMachines =
  res
    "ListStateMachinesResponse"
    "fixture/ListStateMachinesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStateMachines)

responseListExecutions :: ListExecutionsResponse -> TestTree
responseListExecutions =
  res
    "ListExecutionsResponse"
    "fixture/ListExecutionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListExecutions)

responseDeleteStateMachine :: DeleteStateMachineResponse -> TestTree
responseDeleteStateMachine =
  res
    "DeleteStateMachineResponse"
    "fixture/DeleteStateMachineResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStateMachine)

responseUpdateStateMachine :: UpdateStateMachineResponse -> TestTree
responseUpdateStateMachine =
  res
    "UpdateStateMachineResponse"
    "fixture/UpdateStateMachineResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateStateMachine)

responseDescribeStateMachineForExecution :: DescribeStateMachineForExecutionResponse -> TestTree
responseDescribeStateMachineForExecution =
  res
    "DescribeStateMachineForExecutionResponse"
    "fixture/DescribeStateMachineForExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStateMachineForExecution)

responseGetActivityTask :: GetActivityTaskResponse -> TestTree
responseGetActivityTask =
  res
    "GetActivityTaskResponse"
    "fixture/GetActivityTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetActivityTask)

responseCreateActivity :: CreateActivityResponse -> TestTree
responseCreateActivity =
  res
    "CreateActivityResponse"
    "fixture/CreateActivityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateActivity)

responseListActivities :: ListActivitiesResponse -> TestTree
responseListActivities =
  res
    "ListActivitiesResponse"
    "fixture/ListActivitiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListActivities)

responseSendTaskHeartbeat :: SendTaskHeartbeatResponse -> TestTree
responseSendTaskHeartbeat =
  res
    "SendTaskHeartbeatResponse"
    "fixture/SendTaskHeartbeatResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendTaskHeartbeat)

responseSendTaskFailure :: SendTaskFailureResponse -> TestTree
responseSendTaskFailure =
  res
    "SendTaskFailureResponse"
    "fixture/SendTaskFailureResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendTaskFailure)

responseDescribeExecution :: DescribeExecutionResponse -> TestTree
responseDescribeExecution =
  res
    "DescribeExecutionResponse"
    "fixture/DescribeExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeExecution)

responseSendTaskSuccess :: SendTaskSuccessResponse -> TestTree
responseSendTaskSuccess =
  res
    "SendTaskSuccessResponse"
    "fixture/SendTaskSuccessResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendTaskSuccess)

responseStartExecution :: StartExecutionResponse -> TestTree
responseStartExecution =
  res
    "StartExecutionResponse"
    "fixture/StartExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartExecution)

responseStartSyncExecution :: StartSyncExecutionResponse -> TestTree
responseStartSyncExecution =
  res
    "StartSyncExecutionResponse"
    "fixture/StartSyncExecutionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartSyncExecution)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseGetExecutionHistory :: GetExecutionHistoryResponse -> TestTree
responseGetExecutionHistory =
  res
    "GetExecutionHistoryResponse"
    "fixture/GetExecutionHistoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetExecutionHistory)

responseCreateStateMachine :: CreateStateMachineResponse -> TestTree
responseCreateStateMachine =
  res
    "CreateStateMachineResponse"
    "fixture/CreateStateMachineResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStateMachine)
