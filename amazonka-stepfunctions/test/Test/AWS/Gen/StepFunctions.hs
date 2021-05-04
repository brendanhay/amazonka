{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.StepFunctions
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDescribeExecution $
--             newDescribeExecution
--
--         , requestDescribeStateMachine $
--             newDescribeStateMachine
--
--         , requestDeleteActivity $
--             newDeleteActivity
--
--         , requestCreateActivity $
--             newCreateActivity
--
--         , requestListActivities $
--             newListActivities
--
--         , requestCreateStateMachine $
--             newCreateStateMachine
--
--         , requestGetActivityTask $
--             newGetActivityTask
--
--         , requestUpdateStateMachine $
--             newUpdateStateMachine
--
--         , requestListExecutions $
--             newListExecutions
--
--         , requestDeleteStateMachine $
--             newDeleteStateMachine
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeActivity $
--             newDescribeActivity
--
--         , requestTagResource $
--             newTagResource
--
--         , requestStartSyncExecution $
--             newStartSyncExecution
--
--         , requestSendTaskSuccess $
--             newSendTaskSuccess
--
--         , requestSendTaskHeartbeat $
--             newSendTaskHeartbeat
--
--         , requestSendTaskFailure $
--             newSendTaskFailure
--
--         , requestDescribeStateMachineForExecution $
--             newDescribeStateMachineForExecution
--
--         , requestGetExecutionHistory $
--             newGetExecutionHistory
--
--         , requestListStateMachines $
--             newListStateMachines
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStopExecution $
--             newStopExecution
--
--         , requestStartExecution $
--             newStartExecution
--
--           ]

--     , testGroup "response"
--         [ responseDescribeExecution $
--             newDescribeExecutionResponse
--
--         , responseDescribeStateMachine $
--             newDescribeStateMachineResponse
--
--         , responseDeleteActivity $
--             newDeleteActivityResponse
--
--         , responseCreateActivity $
--             newCreateActivityResponse
--
--         , responseListActivities $
--             newListActivitiesResponse
--
--         , responseCreateStateMachine $
--             newCreateStateMachineResponse
--
--         , responseGetActivityTask $
--             newGetActivityTaskResponse
--
--         , responseUpdateStateMachine $
--             newUpdateStateMachineResponse
--
--         , responseListExecutions $
--             newListExecutionsResponse
--
--         , responseDeleteStateMachine $
--             newDeleteStateMachineResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeActivity $
--             newDescribeActivityResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseStartSyncExecution $
--             newStartSyncExecutionResponse
--
--         , responseSendTaskSuccess $
--             newSendTaskSuccessResponse
--
--         , responseSendTaskHeartbeat $
--             newSendTaskHeartbeatResponse
--
--         , responseSendTaskFailure $
--             newSendTaskFailureResponse
--
--         , responseDescribeStateMachineForExecution $
--             newDescribeStateMachineForExecutionResponse
--
--         , responseGetExecutionHistory $
--             newGetExecutionHistoryResponse
--
--         , responseListStateMachines $
--             newListStateMachinesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStopExecution $
--             newStopExecutionResponse
--
--         , responseStartExecution $
--             newStartExecutionResponse
--
--           ]
--     ]

-- Requests

requestDescribeExecution :: DescribeExecution -> TestTree
requestDescribeExecution =
  req
    "DescribeExecution"
    "fixture/DescribeExecution.yaml"

requestDescribeStateMachine :: DescribeStateMachine -> TestTree
requestDescribeStateMachine =
  req
    "DescribeStateMachine"
    "fixture/DescribeStateMachine.yaml"

requestDeleteActivity :: DeleteActivity -> TestTree
requestDeleteActivity =
  req
    "DeleteActivity"
    "fixture/DeleteActivity.yaml"

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

requestCreateStateMachine :: CreateStateMachine -> TestTree
requestCreateStateMachine =
  req
    "CreateStateMachine"
    "fixture/CreateStateMachine.yaml"

requestGetActivityTask :: GetActivityTask -> TestTree
requestGetActivityTask =
  req
    "GetActivityTask"
    "fixture/GetActivityTask.yaml"

requestUpdateStateMachine :: UpdateStateMachine -> TestTree
requestUpdateStateMachine =
  req
    "UpdateStateMachine"
    "fixture/UpdateStateMachine.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeActivity :: DescribeActivity -> TestTree
requestDescribeActivity =
  req
    "DescribeActivity"
    "fixture/DescribeActivity.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestStartSyncExecution :: StartSyncExecution -> TestTree
requestStartSyncExecution =
  req
    "StartSyncExecution"
    "fixture/StartSyncExecution.yaml"

requestSendTaskSuccess :: SendTaskSuccess -> TestTree
requestSendTaskSuccess =
  req
    "SendTaskSuccess"
    "fixture/SendTaskSuccess.yaml"

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

requestDescribeStateMachineForExecution :: DescribeStateMachineForExecution -> TestTree
requestDescribeStateMachineForExecution =
  req
    "DescribeStateMachineForExecution"
    "fixture/DescribeStateMachineForExecution.yaml"

requestGetExecutionHistory :: GetExecutionHistory -> TestTree
requestGetExecutionHistory =
  req
    "GetExecutionHistory"
    "fixture/GetExecutionHistory.yaml"

requestListStateMachines :: ListStateMachines -> TestTree
requestListStateMachines =
  req
    "ListStateMachines"
    "fixture/ListStateMachines.yaml"

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

requestStartExecution :: StartExecution -> TestTree
requestStartExecution =
  req
    "StartExecution"
    "fixture/StartExecution.yaml"

-- Responses

responseDescribeExecution :: DescribeExecutionResponse -> TestTree
responseDescribeExecution =
  res
    "DescribeExecutionResponse"
    "fixture/DescribeExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExecution)

responseDescribeStateMachine :: DescribeStateMachineResponse -> TestTree
responseDescribeStateMachine =
  res
    "DescribeStateMachineResponse"
    "fixture/DescribeStateMachineResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStateMachine)

responseDeleteActivity :: DeleteActivityResponse -> TestTree
responseDeleteActivity =
  res
    "DeleteActivityResponse"
    "fixture/DeleteActivityResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteActivity)

responseCreateActivity :: CreateActivityResponse -> TestTree
responseCreateActivity =
  res
    "CreateActivityResponse"
    "fixture/CreateActivityResponse.proto"
    defaultService
    (Proxy :: Proxy CreateActivity)

responseListActivities :: ListActivitiesResponse -> TestTree
responseListActivities =
  res
    "ListActivitiesResponse"
    "fixture/ListActivitiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListActivities)

responseCreateStateMachine :: CreateStateMachineResponse -> TestTree
responseCreateStateMachine =
  res
    "CreateStateMachineResponse"
    "fixture/CreateStateMachineResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStateMachine)

responseGetActivityTask :: GetActivityTaskResponse -> TestTree
responseGetActivityTask =
  res
    "GetActivityTaskResponse"
    "fixture/GetActivityTaskResponse.proto"
    defaultService
    (Proxy :: Proxy GetActivityTask)

responseUpdateStateMachine :: UpdateStateMachineResponse -> TestTree
responseUpdateStateMachine =
  res
    "UpdateStateMachineResponse"
    "fixture/UpdateStateMachineResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStateMachine)

responseListExecutions :: ListExecutionsResponse -> TestTree
responseListExecutions =
  res
    "ListExecutionsResponse"
    "fixture/ListExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListExecutions)

responseDeleteStateMachine :: DeleteStateMachineResponse -> TestTree
responseDeleteStateMachine =
  res
    "DeleteStateMachineResponse"
    "fixture/DeleteStateMachineResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStateMachine)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeActivity :: DescribeActivityResponse -> TestTree
responseDescribeActivity =
  res
    "DescribeActivityResponse"
    "fixture/DescribeActivityResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeActivity)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseStartSyncExecution :: StartSyncExecutionResponse -> TestTree
responseStartSyncExecution =
  res
    "StartSyncExecutionResponse"
    "fixture/StartSyncExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartSyncExecution)

responseSendTaskSuccess :: SendTaskSuccessResponse -> TestTree
responseSendTaskSuccess =
  res
    "SendTaskSuccessResponse"
    "fixture/SendTaskSuccessResponse.proto"
    defaultService
    (Proxy :: Proxy SendTaskSuccess)

responseSendTaskHeartbeat :: SendTaskHeartbeatResponse -> TestTree
responseSendTaskHeartbeat =
  res
    "SendTaskHeartbeatResponse"
    "fixture/SendTaskHeartbeatResponse.proto"
    defaultService
    (Proxy :: Proxy SendTaskHeartbeat)

responseSendTaskFailure :: SendTaskFailureResponse -> TestTree
responseSendTaskFailure =
  res
    "SendTaskFailureResponse"
    "fixture/SendTaskFailureResponse.proto"
    defaultService
    (Proxy :: Proxy SendTaskFailure)

responseDescribeStateMachineForExecution :: DescribeStateMachineForExecutionResponse -> TestTree
responseDescribeStateMachineForExecution =
  res
    "DescribeStateMachineForExecutionResponse"
    "fixture/DescribeStateMachineForExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStateMachineForExecution)

responseGetExecutionHistory :: GetExecutionHistoryResponse -> TestTree
responseGetExecutionHistory =
  res
    "GetExecutionHistoryResponse"
    "fixture/GetExecutionHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetExecutionHistory)

responseListStateMachines :: ListStateMachinesResponse -> TestTree
responseListStateMachines =
  res
    "ListStateMachinesResponse"
    "fixture/ListStateMachinesResponse.proto"
    defaultService
    (Proxy :: Proxy ListStateMachines)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseStopExecution :: StopExecutionResponse -> TestTree
responseStopExecution =
  res
    "StopExecutionResponse"
    "fixture/StopExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StopExecution)

responseStartExecution :: StartExecutionResponse -> TestTree
responseStartExecution =
  res
    "StartExecutionResponse"
    "fixture/StartExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartExecution)
