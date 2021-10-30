{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.DataSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.DataSync where

import qualified Data.Proxy as Proxy
import Network.AWS.DataSync
import Test.AWS.DataSync.Internal
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
--         [ requestUpdateTask $
--             newUpdateTask
--
--         , requestDescribeAgent $
--             newDescribeAgent
--
--         , requestDeleteTask $
--             newDeleteTask
--
--         , requestDescribeLocationSmb $
--             newDescribeLocationSmb
--
--         , requestListLocations $
--             newListLocations
--
--         , requestCreateLocationNfs $
--             newCreateLocationNfs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeLocationFsxWindows $
--             newDescribeLocationFsxWindows
--
--         , requestCreateLocationObjectStorage $
--             newCreateLocationObjectStorage
--
--         , requestDescribeTask $
--             newDescribeTask
--
--         , requestDescribeLocationS3 $
--             newDescribeLocationS3
--
--         , requestListAgents $
--             newListAgents
--
--         , requestUpdateLocationSmb $
--             newUpdateLocationSmb
--
--         , requestDeleteAgent $
--             newDeleteAgent
--
--         , requestUpdateAgent $
--             newUpdateAgent
--
--         , requestCreateLocationFsxWindows $
--             newCreateLocationFsxWindows
--
--         , requestListTaskExecutions $
--             newListTaskExecutions
--
--         , requestUpdateTaskExecution $
--             newUpdateTaskExecution
--
--         , requestCreateLocationS3 $
--             newCreateLocationS3
--
--         , requestCreateTask $
--             newCreateTask
--
--         , requestCreateLocationEfs $
--             newCreateLocationEfs
--
--         , requestDescribeLocationObjectStorage $
--             newDescribeLocationObjectStorage
--
--         , requestDeleteLocation $
--             newDeleteLocation
--
--         , requestListTasks $
--             newListTasks
--
--         , requestStartTaskExecution $
--             newStartTaskExecution
--
--         , requestUpdateLocationNfs $
--             newUpdateLocationNfs
--
--         , requestDescribeTaskExecution $
--             newDescribeTaskExecution
--
--         , requestCreateLocationSmb $
--             newCreateLocationSmb
--
--         , requestCreateAgent $
--             newCreateAgent
--
--         , requestUpdateLocationObjectStorage $
--             newUpdateLocationObjectStorage
--
--         , requestDescribeLocationEfs $
--             newDescribeLocationEfs
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeLocationNfs $
--             newDescribeLocationNfs
--
--         , requestCancelTaskExecution $
--             newCancelTaskExecution
--
--           ]

--     , testGroup "response"
--         [ responseUpdateTask $
--             newUpdateTaskResponse
--
--         , responseDescribeAgent $
--             newDescribeAgentResponse
--
--         , responseDeleteTask $
--             newDeleteTaskResponse
--
--         , responseDescribeLocationSmb $
--             newDescribeLocationSmbResponse
--
--         , responseListLocations $
--             newListLocationsResponse
--
--         , responseCreateLocationNfs $
--             newCreateLocationNfsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeLocationFsxWindows $
--             newDescribeLocationFsxWindowsResponse
--
--         , responseCreateLocationObjectStorage $
--             newCreateLocationObjectStorageResponse
--
--         , responseDescribeTask $
--             newDescribeTaskResponse
--
--         , responseDescribeLocationS3 $
--             newDescribeLocationS3Response
--
--         , responseListAgents $
--             newListAgentsResponse
--
--         , responseUpdateLocationSmb $
--             newUpdateLocationSmbResponse
--
--         , responseDeleteAgent $
--             newDeleteAgentResponse
--
--         , responseUpdateAgent $
--             newUpdateAgentResponse
--
--         , responseCreateLocationFsxWindows $
--             newCreateLocationFsxWindowsResponse
--
--         , responseListTaskExecutions $
--             newListTaskExecutionsResponse
--
--         , responseUpdateTaskExecution $
--             newUpdateTaskExecutionResponse
--
--         , responseCreateLocationS3 $
--             newCreateLocationS3Response
--
--         , responseCreateTask $
--             newCreateTaskResponse
--
--         , responseCreateLocationEfs $
--             newCreateLocationEfsResponse
--
--         , responseDescribeLocationObjectStorage $
--             newDescribeLocationObjectStorageResponse
--
--         , responseDeleteLocation $
--             newDeleteLocationResponse
--
--         , responseListTasks $
--             newListTasksResponse
--
--         , responseStartTaskExecution $
--             newStartTaskExecutionResponse
--
--         , responseUpdateLocationNfs $
--             newUpdateLocationNfsResponse
--
--         , responseDescribeTaskExecution $
--             newDescribeTaskExecutionResponse
--
--         , responseCreateLocationSmb $
--             newCreateLocationSmbResponse
--
--         , responseCreateAgent $
--             newCreateAgentResponse
--
--         , responseUpdateLocationObjectStorage $
--             newUpdateLocationObjectStorageResponse
--
--         , responseDescribeLocationEfs $
--             newDescribeLocationEfsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeLocationNfs $
--             newDescribeLocationNfsResponse
--
--         , responseCancelTaskExecution $
--             newCancelTaskExecutionResponse
--
--           ]
--     ]

-- Requests

requestUpdateTask :: UpdateTask -> TestTree
requestUpdateTask =
  req
    "UpdateTask"
    "fixture/UpdateTask.yaml"

requestDescribeAgent :: DescribeAgent -> TestTree
requestDescribeAgent =
  req
    "DescribeAgent"
    "fixture/DescribeAgent.yaml"

requestDeleteTask :: DeleteTask -> TestTree
requestDeleteTask =
  req
    "DeleteTask"
    "fixture/DeleteTask.yaml"

requestDescribeLocationSmb :: DescribeLocationSmb -> TestTree
requestDescribeLocationSmb =
  req
    "DescribeLocationSmb"
    "fixture/DescribeLocationSmb.yaml"

requestListLocations :: ListLocations -> TestTree
requestListLocations =
  req
    "ListLocations"
    "fixture/ListLocations.yaml"

requestCreateLocationNfs :: CreateLocationNfs -> TestTree
requestCreateLocationNfs =
  req
    "CreateLocationNfs"
    "fixture/CreateLocationNfs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeLocationFsxWindows :: DescribeLocationFsxWindows -> TestTree
requestDescribeLocationFsxWindows =
  req
    "DescribeLocationFsxWindows"
    "fixture/DescribeLocationFsxWindows.yaml"

requestCreateLocationObjectStorage :: CreateLocationObjectStorage -> TestTree
requestCreateLocationObjectStorage =
  req
    "CreateLocationObjectStorage"
    "fixture/CreateLocationObjectStorage.yaml"

requestDescribeTask :: DescribeTask -> TestTree
requestDescribeTask =
  req
    "DescribeTask"
    "fixture/DescribeTask.yaml"

requestDescribeLocationS3 :: DescribeLocationS3 -> TestTree
requestDescribeLocationS3 =
  req
    "DescribeLocationS3"
    "fixture/DescribeLocationS3.yaml"

requestListAgents :: ListAgents -> TestTree
requestListAgents =
  req
    "ListAgents"
    "fixture/ListAgents.yaml"

requestUpdateLocationSmb :: UpdateLocationSmb -> TestTree
requestUpdateLocationSmb =
  req
    "UpdateLocationSmb"
    "fixture/UpdateLocationSmb.yaml"

requestDeleteAgent :: DeleteAgent -> TestTree
requestDeleteAgent =
  req
    "DeleteAgent"
    "fixture/DeleteAgent.yaml"

requestUpdateAgent :: UpdateAgent -> TestTree
requestUpdateAgent =
  req
    "UpdateAgent"
    "fixture/UpdateAgent.yaml"

requestCreateLocationFsxWindows :: CreateLocationFsxWindows -> TestTree
requestCreateLocationFsxWindows =
  req
    "CreateLocationFsxWindows"
    "fixture/CreateLocationFsxWindows.yaml"

requestListTaskExecutions :: ListTaskExecutions -> TestTree
requestListTaskExecutions =
  req
    "ListTaskExecutions"
    "fixture/ListTaskExecutions.yaml"

requestUpdateTaskExecution :: UpdateTaskExecution -> TestTree
requestUpdateTaskExecution =
  req
    "UpdateTaskExecution"
    "fixture/UpdateTaskExecution.yaml"

requestCreateLocationS3 :: CreateLocationS3 -> TestTree
requestCreateLocationS3 =
  req
    "CreateLocationS3"
    "fixture/CreateLocationS3.yaml"

requestCreateTask :: CreateTask -> TestTree
requestCreateTask =
  req
    "CreateTask"
    "fixture/CreateTask.yaml"

requestCreateLocationEfs :: CreateLocationEfs -> TestTree
requestCreateLocationEfs =
  req
    "CreateLocationEfs"
    "fixture/CreateLocationEfs.yaml"

requestDescribeLocationObjectStorage :: DescribeLocationObjectStorage -> TestTree
requestDescribeLocationObjectStorage =
  req
    "DescribeLocationObjectStorage"
    "fixture/DescribeLocationObjectStorage.yaml"

requestDeleteLocation :: DeleteLocation -> TestTree
requestDeleteLocation =
  req
    "DeleteLocation"
    "fixture/DeleteLocation.yaml"

requestListTasks :: ListTasks -> TestTree
requestListTasks =
  req
    "ListTasks"
    "fixture/ListTasks.yaml"

requestStartTaskExecution :: StartTaskExecution -> TestTree
requestStartTaskExecution =
  req
    "StartTaskExecution"
    "fixture/StartTaskExecution.yaml"

requestUpdateLocationNfs :: UpdateLocationNfs -> TestTree
requestUpdateLocationNfs =
  req
    "UpdateLocationNfs"
    "fixture/UpdateLocationNfs.yaml"

requestDescribeTaskExecution :: DescribeTaskExecution -> TestTree
requestDescribeTaskExecution =
  req
    "DescribeTaskExecution"
    "fixture/DescribeTaskExecution.yaml"

requestCreateLocationSmb :: CreateLocationSmb -> TestTree
requestCreateLocationSmb =
  req
    "CreateLocationSmb"
    "fixture/CreateLocationSmb.yaml"

requestCreateAgent :: CreateAgent -> TestTree
requestCreateAgent =
  req
    "CreateAgent"
    "fixture/CreateAgent.yaml"

requestUpdateLocationObjectStorage :: UpdateLocationObjectStorage -> TestTree
requestUpdateLocationObjectStorage =
  req
    "UpdateLocationObjectStorage"
    "fixture/UpdateLocationObjectStorage.yaml"

requestDescribeLocationEfs :: DescribeLocationEfs -> TestTree
requestDescribeLocationEfs =
  req
    "DescribeLocationEfs"
    "fixture/DescribeLocationEfs.yaml"

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

requestDescribeLocationNfs :: DescribeLocationNfs -> TestTree
requestDescribeLocationNfs =
  req
    "DescribeLocationNfs"
    "fixture/DescribeLocationNfs.yaml"

requestCancelTaskExecution :: CancelTaskExecution -> TestTree
requestCancelTaskExecution =
  req
    "CancelTaskExecution"
    "fixture/CancelTaskExecution.yaml"

-- Responses

responseUpdateTask :: UpdateTaskResponse -> TestTree
responseUpdateTask =
  res
    "UpdateTaskResponse"
    "fixture/UpdateTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTask)

responseDescribeAgent :: DescribeAgentResponse -> TestTree
responseDescribeAgent =
  res
    "DescribeAgentResponse"
    "fixture/DescribeAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAgent)

responseDeleteTask :: DeleteTaskResponse -> TestTree
responseDeleteTask =
  res
    "DeleteTaskResponse"
    "fixture/DeleteTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTask)

responseDescribeLocationSmb :: DescribeLocationSmbResponse -> TestTree
responseDescribeLocationSmb =
  res
    "DescribeLocationSmbResponse"
    "fixture/DescribeLocationSmbResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationSmb)

responseListLocations :: ListLocationsResponse -> TestTree
responseListLocations =
  res
    "ListLocationsResponse"
    "fixture/ListLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLocations)

responseCreateLocationNfs :: CreateLocationNfsResponse -> TestTree
responseCreateLocationNfs =
  res
    "CreateLocationNfsResponse"
    "fixture/CreateLocationNfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationNfs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribeLocationFsxWindows :: DescribeLocationFsxWindowsResponse -> TestTree
responseDescribeLocationFsxWindows =
  res
    "DescribeLocationFsxWindowsResponse"
    "fixture/DescribeLocationFsxWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationFsxWindows)

responseCreateLocationObjectStorage :: CreateLocationObjectStorageResponse -> TestTree
responseCreateLocationObjectStorage =
  res
    "CreateLocationObjectStorageResponse"
    "fixture/CreateLocationObjectStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationObjectStorage)

responseDescribeTask :: DescribeTaskResponse -> TestTree
responseDescribeTask =
  res
    "DescribeTaskResponse"
    "fixture/DescribeTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTask)

responseDescribeLocationS3 :: DescribeLocationS3Response -> TestTree
responseDescribeLocationS3 =
  res
    "DescribeLocationS3Response"
    "fixture/DescribeLocationS3Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationS3)

responseListAgents :: ListAgentsResponse -> TestTree
responseListAgents =
  res
    "ListAgentsResponse"
    "fixture/ListAgentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAgents)

responseUpdateLocationSmb :: UpdateLocationSmbResponse -> TestTree
responseUpdateLocationSmb =
  res
    "UpdateLocationSmbResponse"
    "fixture/UpdateLocationSmbResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLocationSmb)

responseDeleteAgent :: DeleteAgentResponse -> TestTree
responseDeleteAgent =
  res
    "DeleteAgentResponse"
    "fixture/DeleteAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAgent)

responseUpdateAgent :: UpdateAgentResponse -> TestTree
responseUpdateAgent =
  res
    "UpdateAgentResponse"
    "fixture/UpdateAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAgent)

responseCreateLocationFsxWindows :: CreateLocationFsxWindowsResponse -> TestTree
responseCreateLocationFsxWindows =
  res
    "CreateLocationFsxWindowsResponse"
    "fixture/CreateLocationFsxWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationFsxWindows)

responseListTaskExecutions :: ListTaskExecutionsResponse -> TestTree
responseListTaskExecutions =
  res
    "ListTaskExecutionsResponse"
    "fixture/ListTaskExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTaskExecutions)

responseUpdateTaskExecution :: UpdateTaskExecutionResponse -> TestTree
responseUpdateTaskExecution =
  res
    "UpdateTaskExecutionResponse"
    "fixture/UpdateTaskExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTaskExecution)

responseCreateLocationS3 :: CreateLocationS3Response -> TestTree
responseCreateLocationS3 =
  res
    "CreateLocationS3Response"
    "fixture/CreateLocationS3Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationS3)

responseCreateTask :: CreateTaskResponse -> TestTree
responseCreateTask =
  res
    "CreateTaskResponse"
    "fixture/CreateTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTask)

responseCreateLocationEfs :: CreateLocationEfsResponse -> TestTree
responseCreateLocationEfs =
  res
    "CreateLocationEfsResponse"
    "fixture/CreateLocationEfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationEfs)

responseDescribeLocationObjectStorage :: DescribeLocationObjectStorageResponse -> TestTree
responseDescribeLocationObjectStorage =
  res
    "DescribeLocationObjectStorageResponse"
    "fixture/DescribeLocationObjectStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationObjectStorage)

responseDeleteLocation :: DeleteLocationResponse -> TestTree
responseDeleteLocation =
  res
    "DeleteLocationResponse"
    "fixture/DeleteLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLocation)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks =
  res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTasks)

responseStartTaskExecution :: StartTaskExecutionResponse -> TestTree
responseStartTaskExecution =
  res
    "StartTaskExecutionResponse"
    "fixture/StartTaskExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTaskExecution)

responseUpdateLocationNfs :: UpdateLocationNfsResponse -> TestTree
responseUpdateLocationNfs =
  res
    "UpdateLocationNfsResponse"
    "fixture/UpdateLocationNfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLocationNfs)

responseDescribeTaskExecution :: DescribeTaskExecutionResponse -> TestTree
responseDescribeTaskExecution =
  res
    "DescribeTaskExecutionResponse"
    "fixture/DescribeTaskExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTaskExecution)

responseCreateLocationSmb :: CreateLocationSmbResponse -> TestTree
responseCreateLocationSmb =
  res
    "CreateLocationSmbResponse"
    "fixture/CreateLocationSmbResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationSmb)

responseCreateAgent :: CreateAgentResponse -> TestTree
responseCreateAgent =
  res
    "CreateAgentResponse"
    "fixture/CreateAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAgent)

responseUpdateLocationObjectStorage :: UpdateLocationObjectStorageResponse -> TestTree
responseUpdateLocationObjectStorage =
  res
    "UpdateLocationObjectStorageResponse"
    "fixture/UpdateLocationObjectStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLocationObjectStorage)

responseDescribeLocationEfs :: DescribeLocationEfsResponse -> TestTree
responseDescribeLocationEfs =
  res
    "DescribeLocationEfsResponse"
    "fixture/DescribeLocationEfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationEfs)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeLocationNfs :: DescribeLocationNfsResponse -> TestTree
responseDescribeLocationNfs =
  res
    "DescribeLocationNfsResponse"
    "fixture/DescribeLocationNfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationNfs)

responseCancelTaskExecution :: CancelTaskExecutionResponse -> TestTree
responseCancelTaskExecution =
  res
    "CancelTaskExecutionResponse"
    "fixture/CancelTaskExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelTaskExecution)
