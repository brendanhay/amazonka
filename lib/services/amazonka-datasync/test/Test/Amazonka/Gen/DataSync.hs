{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DataSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.DataSync where

import Amazonka.DataSync
import qualified Data.Proxy as Proxy
import Test.Amazonka.DataSync.Internal
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
--         [ requestCancelTaskExecution $
--             newCancelTaskExecution
--
--         , requestCreateAgent $
--             newCreateAgent
--
--         , requestCreateLocationEfs $
--             newCreateLocationEfs
--
--         , requestCreateLocationFsxWindows $
--             newCreateLocationFsxWindows
--
--         , requestCreateLocationNfs $
--             newCreateLocationNfs
--
--         , requestCreateLocationObjectStorage $
--             newCreateLocationObjectStorage
--
--         , requestCreateLocationS3 $
--             newCreateLocationS3
--
--         , requestCreateLocationSmb $
--             newCreateLocationSmb
--
--         , requestCreateTask $
--             newCreateTask
--
--         , requestDeleteAgent $
--             newDeleteAgent
--
--         , requestDeleteLocation $
--             newDeleteLocation
--
--         , requestDeleteTask $
--             newDeleteTask
--
--         , requestDescribeAgent $
--             newDescribeAgent
--
--         , requestDescribeLocationEfs $
--             newDescribeLocationEfs
--
--         , requestDescribeLocationFsxWindows $
--             newDescribeLocationFsxWindows
--
--         , requestDescribeLocationNfs $
--             newDescribeLocationNfs
--
--         , requestDescribeLocationObjectStorage $
--             newDescribeLocationObjectStorage
--
--         , requestDescribeLocationS3 $
--             newDescribeLocationS3
--
--         , requestDescribeLocationSmb $
--             newDescribeLocationSmb
--
--         , requestDescribeTask $
--             newDescribeTask
--
--         , requestDescribeTaskExecution $
--             newDescribeTaskExecution
--
--         , requestListAgents $
--             newListAgents
--
--         , requestListLocations $
--             newListLocations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTaskExecutions $
--             newListTaskExecutions
--
--         , requestListTasks $
--             newListTasks
--
--         , requestStartTaskExecution $
--             newStartTaskExecution
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAgent $
--             newUpdateAgent
--
--         , requestUpdateLocationNfs $
--             newUpdateLocationNfs
--
--         , requestUpdateLocationObjectStorage $
--             newUpdateLocationObjectStorage
--
--         , requestUpdateLocationSmb $
--             newUpdateLocationSmb
--
--         , requestUpdateTask $
--             newUpdateTask
--
--         , requestUpdateTaskExecution $
--             newUpdateTaskExecution
--
--           ]

--     , testGroup "response"
--         [ responseCancelTaskExecution $
--             newCancelTaskExecutionResponse
--
--         , responseCreateAgent $
--             newCreateAgentResponse
--
--         , responseCreateLocationEfs $
--             newCreateLocationEfsResponse
--
--         , responseCreateLocationFsxWindows $
--             newCreateLocationFsxWindowsResponse
--
--         , responseCreateLocationNfs $
--             newCreateLocationNfsResponse
--
--         , responseCreateLocationObjectStorage $
--             newCreateLocationObjectStorageResponse
--
--         , responseCreateLocationS3 $
--             newCreateLocationS3Response
--
--         , responseCreateLocationSmb $
--             newCreateLocationSmbResponse
--
--         , responseCreateTask $
--             newCreateTaskResponse
--
--         , responseDeleteAgent $
--             newDeleteAgentResponse
--
--         , responseDeleteLocation $
--             newDeleteLocationResponse
--
--         , responseDeleteTask $
--             newDeleteTaskResponse
--
--         , responseDescribeAgent $
--             newDescribeAgentResponse
--
--         , responseDescribeLocationEfs $
--             newDescribeLocationEfsResponse
--
--         , responseDescribeLocationFsxWindows $
--             newDescribeLocationFsxWindowsResponse
--
--         , responseDescribeLocationNfs $
--             newDescribeLocationNfsResponse
--
--         , responseDescribeLocationObjectStorage $
--             newDescribeLocationObjectStorageResponse
--
--         , responseDescribeLocationS3 $
--             newDescribeLocationS3Response
--
--         , responseDescribeLocationSmb $
--             newDescribeLocationSmbResponse
--
--         , responseDescribeTask $
--             newDescribeTaskResponse
--
--         , responseDescribeTaskExecution $
--             newDescribeTaskExecutionResponse
--
--         , responseListAgents $
--             newListAgentsResponse
--
--         , responseListLocations $
--             newListLocationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTaskExecutions $
--             newListTaskExecutionsResponse
--
--         , responseListTasks $
--             newListTasksResponse
--
--         , responseStartTaskExecution $
--             newStartTaskExecutionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAgent $
--             newUpdateAgentResponse
--
--         , responseUpdateLocationNfs $
--             newUpdateLocationNfsResponse
--
--         , responseUpdateLocationObjectStorage $
--             newUpdateLocationObjectStorageResponse
--
--         , responseUpdateLocationSmb $
--             newUpdateLocationSmbResponse
--
--         , responseUpdateTask $
--             newUpdateTaskResponse
--
--         , responseUpdateTaskExecution $
--             newUpdateTaskExecutionResponse
--
--           ]
--     ]

-- Requests

requestCancelTaskExecution :: CancelTaskExecution -> TestTree
requestCancelTaskExecution =
  req
    "CancelTaskExecution"
    "fixture/CancelTaskExecution.yaml"

requestCreateAgent :: CreateAgent -> TestTree
requestCreateAgent =
  req
    "CreateAgent"
    "fixture/CreateAgent.yaml"

requestCreateLocationEfs :: CreateLocationEfs -> TestTree
requestCreateLocationEfs =
  req
    "CreateLocationEfs"
    "fixture/CreateLocationEfs.yaml"

requestCreateLocationFsxWindows :: CreateLocationFsxWindows -> TestTree
requestCreateLocationFsxWindows =
  req
    "CreateLocationFsxWindows"
    "fixture/CreateLocationFsxWindows.yaml"

requestCreateLocationNfs :: CreateLocationNfs -> TestTree
requestCreateLocationNfs =
  req
    "CreateLocationNfs"
    "fixture/CreateLocationNfs.yaml"

requestCreateLocationObjectStorage :: CreateLocationObjectStorage -> TestTree
requestCreateLocationObjectStorage =
  req
    "CreateLocationObjectStorage"
    "fixture/CreateLocationObjectStorage.yaml"

requestCreateLocationS3 :: CreateLocationS3 -> TestTree
requestCreateLocationS3 =
  req
    "CreateLocationS3"
    "fixture/CreateLocationS3.yaml"

requestCreateLocationSmb :: CreateLocationSmb -> TestTree
requestCreateLocationSmb =
  req
    "CreateLocationSmb"
    "fixture/CreateLocationSmb.yaml"

requestCreateTask :: CreateTask -> TestTree
requestCreateTask =
  req
    "CreateTask"
    "fixture/CreateTask.yaml"

requestDeleteAgent :: DeleteAgent -> TestTree
requestDeleteAgent =
  req
    "DeleteAgent"
    "fixture/DeleteAgent.yaml"

requestDeleteLocation :: DeleteLocation -> TestTree
requestDeleteLocation =
  req
    "DeleteLocation"
    "fixture/DeleteLocation.yaml"

requestDeleteTask :: DeleteTask -> TestTree
requestDeleteTask =
  req
    "DeleteTask"
    "fixture/DeleteTask.yaml"

requestDescribeAgent :: DescribeAgent -> TestTree
requestDescribeAgent =
  req
    "DescribeAgent"
    "fixture/DescribeAgent.yaml"

requestDescribeLocationEfs :: DescribeLocationEfs -> TestTree
requestDescribeLocationEfs =
  req
    "DescribeLocationEfs"
    "fixture/DescribeLocationEfs.yaml"

requestDescribeLocationFsxWindows :: DescribeLocationFsxWindows -> TestTree
requestDescribeLocationFsxWindows =
  req
    "DescribeLocationFsxWindows"
    "fixture/DescribeLocationFsxWindows.yaml"

requestDescribeLocationNfs :: DescribeLocationNfs -> TestTree
requestDescribeLocationNfs =
  req
    "DescribeLocationNfs"
    "fixture/DescribeLocationNfs.yaml"

requestDescribeLocationObjectStorage :: DescribeLocationObjectStorage -> TestTree
requestDescribeLocationObjectStorage =
  req
    "DescribeLocationObjectStorage"
    "fixture/DescribeLocationObjectStorage.yaml"

requestDescribeLocationS3 :: DescribeLocationS3 -> TestTree
requestDescribeLocationS3 =
  req
    "DescribeLocationS3"
    "fixture/DescribeLocationS3.yaml"

requestDescribeLocationSmb :: DescribeLocationSmb -> TestTree
requestDescribeLocationSmb =
  req
    "DescribeLocationSmb"
    "fixture/DescribeLocationSmb.yaml"

requestDescribeTask :: DescribeTask -> TestTree
requestDescribeTask =
  req
    "DescribeTask"
    "fixture/DescribeTask.yaml"

requestDescribeTaskExecution :: DescribeTaskExecution -> TestTree
requestDescribeTaskExecution =
  req
    "DescribeTaskExecution"
    "fixture/DescribeTaskExecution.yaml"

requestListAgents :: ListAgents -> TestTree
requestListAgents =
  req
    "ListAgents"
    "fixture/ListAgents.yaml"

requestListLocations :: ListLocations -> TestTree
requestListLocations =
  req
    "ListLocations"
    "fixture/ListLocations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTaskExecutions :: ListTaskExecutions -> TestTree
requestListTaskExecutions =
  req
    "ListTaskExecutions"
    "fixture/ListTaskExecutions.yaml"

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

requestUpdateAgent :: UpdateAgent -> TestTree
requestUpdateAgent =
  req
    "UpdateAgent"
    "fixture/UpdateAgent.yaml"

requestUpdateLocationNfs :: UpdateLocationNfs -> TestTree
requestUpdateLocationNfs =
  req
    "UpdateLocationNfs"
    "fixture/UpdateLocationNfs.yaml"

requestUpdateLocationObjectStorage :: UpdateLocationObjectStorage -> TestTree
requestUpdateLocationObjectStorage =
  req
    "UpdateLocationObjectStorage"
    "fixture/UpdateLocationObjectStorage.yaml"

requestUpdateLocationSmb :: UpdateLocationSmb -> TestTree
requestUpdateLocationSmb =
  req
    "UpdateLocationSmb"
    "fixture/UpdateLocationSmb.yaml"

requestUpdateTask :: UpdateTask -> TestTree
requestUpdateTask =
  req
    "UpdateTask"
    "fixture/UpdateTask.yaml"

requestUpdateTaskExecution :: UpdateTaskExecution -> TestTree
requestUpdateTaskExecution =
  req
    "UpdateTaskExecution"
    "fixture/UpdateTaskExecution.yaml"

-- Responses

responseCancelTaskExecution :: CancelTaskExecutionResponse -> TestTree
responseCancelTaskExecution =
  res
    "CancelTaskExecutionResponse"
    "fixture/CancelTaskExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelTaskExecution)

responseCreateAgent :: CreateAgentResponse -> TestTree
responseCreateAgent =
  res
    "CreateAgentResponse"
    "fixture/CreateAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAgent)

responseCreateLocationEfs :: CreateLocationEfsResponse -> TestTree
responseCreateLocationEfs =
  res
    "CreateLocationEfsResponse"
    "fixture/CreateLocationEfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationEfs)

responseCreateLocationFsxWindows :: CreateLocationFsxWindowsResponse -> TestTree
responseCreateLocationFsxWindows =
  res
    "CreateLocationFsxWindowsResponse"
    "fixture/CreateLocationFsxWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationFsxWindows)

responseCreateLocationNfs :: CreateLocationNfsResponse -> TestTree
responseCreateLocationNfs =
  res
    "CreateLocationNfsResponse"
    "fixture/CreateLocationNfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationNfs)

responseCreateLocationObjectStorage :: CreateLocationObjectStorageResponse -> TestTree
responseCreateLocationObjectStorage =
  res
    "CreateLocationObjectStorageResponse"
    "fixture/CreateLocationObjectStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationObjectStorage)

responseCreateLocationS3 :: CreateLocationS3Response -> TestTree
responseCreateLocationS3 =
  res
    "CreateLocationS3Response"
    "fixture/CreateLocationS3Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationS3)

responseCreateLocationSmb :: CreateLocationSmbResponse -> TestTree
responseCreateLocationSmb =
  res
    "CreateLocationSmbResponse"
    "fixture/CreateLocationSmbResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationSmb)

responseCreateTask :: CreateTaskResponse -> TestTree
responseCreateTask =
  res
    "CreateTaskResponse"
    "fixture/CreateTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTask)

responseDeleteAgent :: DeleteAgentResponse -> TestTree
responseDeleteAgent =
  res
    "DeleteAgentResponse"
    "fixture/DeleteAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAgent)

responseDeleteLocation :: DeleteLocationResponse -> TestTree
responseDeleteLocation =
  res
    "DeleteLocationResponse"
    "fixture/DeleteLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLocation)

responseDeleteTask :: DeleteTaskResponse -> TestTree
responseDeleteTask =
  res
    "DeleteTaskResponse"
    "fixture/DeleteTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTask)

responseDescribeAgent :: DescribeAgentResponse -> TestTree
responseDescribeAgent =
  res
    "DescribeAgentResponse"
    "fixture/DescribeAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAgent)

responseDescribeLocationEfs :: DescribeLocationEfsResponse -> TestTree
responseDescribeLocationEfs =
  res
    "DescribeLocationEfsResponse"
    "fixture/DescribeLocationEfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationEfs)

responseDescribeLocationFsxWindows :: DescribeLocationFsxWindowsResponse -> TestTree
responseDescribeLocationFsxWindows =
  res
    "DescribeLocationFsxWindowsResponse"
    "fixture/DescribeLocationFsxWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationFsxWindows)

responseDescribeLocationNfs :: DescribeLocationNfsResponse -> TestTree
responseDescribeLocationNfs =
  res
    "DescribeLocationNfsResponse"
    "fixture/DescribeLocationNfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationNfs)

responseDescribeLocationObjectStorage :: DescribeLocationObjectStorageResponse -> TestTree
responseDescribeLocationObjectStorage =
  res
    "DescribeLocationObjectStorageResponse"
    "fixture/DescribeLocationObjectStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationObjectStorage)

responseDescribeLocationS3 :: DescribeLocationS3Response -> TestTree
responseDescribeLocationS3 =
  res
    "DescribeLocationS3Response"
    "fixture/DescribeLocationS3Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationS3)

responseDescribeLocationSmb :: DescribeLocationSmbResponse -> TestTree
responseDescribeLocationSmb =
  res
    "DescribeLocationSmbResponse"
    "fixture/DescribeLocationSmbResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationSmb)

responseDescribeTask :: DescribeTaskResponse -> TestTree
responseDescribeTask =
  res
    "DescribeTaskResponse"
    "fixture/DescribeTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTask)

responseDescribeTaskExecution :: DescribeTaskExecutionResponse -> TestTree
responseDescribeTaskExecution =
  res
    "DescribeTaskExecutionResponse"
    "fixture/DescribeTaskExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTaskExecution)

responseListAgents :: ListAgentsResponse -> TestTree
responseListAgents =
  res
    "ListAgentsResponse"
    "fixture/ListAgentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAgents)

responseListLocations :: ListLocationsResponse -> TestTree
responseListLocations =
  res
    "ListLocationsResponse"
    "fixture/ListLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLocations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTaskExecutions :: ListTaskExecutionsResponse -> TestTree
responseListTaskExecutions =
  res
    "ListTaskExecutionsResponse"
    "fixture/ListTaskExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTaskExecutions)

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

responseUpdateAgent :: UpdateAgentResponse -> TestTree
responseUpdateAgent =
  res
    "UpdateAgentResponse"
    "fixture/UpdateAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAgent)

responseUpdateLocationNfs :: UpdateLocationNfsResponse -> TestTree
responseUpdateLocationNfs =
  res
    "UpdateLocationNfsResponse"
    "fixture/UpdateLocationNfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLocationNfs)

responseUpdateLocationObjectStorage :: UpdateLocationObjectStorageResponse -> TestTree
responseUpdateLocationObjectStorage =
  res
    "UpdateLocationObjectStorageResponse"
    "fixture/UpdateLocationObjectStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLocationObjectStorage)

responseUpdateLocationSmb :: UpdateLocationSmbResponse -> TestTree
responseUpdateLocationSmb =
  res
    "UpdateLocationSmbResponse"
    "fixture/UpdateLocationSmbResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLocationSmb)

responseUpdateTask :: UpdateTaskResponse -> TestTree
responseUpdateTask =
  res
    "UpdateTaskResponse"
    "fixture/UpdateTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTask)

responseUpdateTaskExecution :: UpdateTaskExecutionResponse -> TestTree
responseUpdateTaskExecution =
  res
    "UpdateTaskExecutionResponse"
    "fixture/UpdateTaskExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTaskExecution)
