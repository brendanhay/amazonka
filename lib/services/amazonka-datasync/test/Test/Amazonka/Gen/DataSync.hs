{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.DataSync
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestAddStorageSystem $
--             newAddStorageSystem
--
--         , requestCancelTaskExecution $
--             newCancelTaskExecution
--
--         , requestCreateAgent $
--             newCreateAgent
--
--         , requestCreateLocationEfs $
--             newCreateLocationEfs
--
--         , requestCreateLocationFsxLustre $
--             newCreateLocationFsxLustre
--
--         , requestCreateLocationFsxOntap $
--             newCreateLocationFsxOntap
--
--         , requestCreateLocationFsxOpenZfs $
--             newCreateLocationFsxOpenZfs
--
--         , requestCreateLocationFsxWindows $
--             newCreateLocationFsxWindows
--
--         , requestCreateLocationHdfs $
--             newCreateLocationHdfs
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
--         , requestDescribeDiscoveryJob $
--             newDescribeDiscoveryJob
--
--         , requestDescribeLocationEfs $
--             newDescribeLocationEfs
--
--         , requestDescribeLocationFsxLustre $
--             newDescribeLocationFsxLustre
--
--         , requestDescribeLocationFsxOntap $
--             newDescribeLocationFsxOntap
--
--         , requestDescribeLocationFsxOpenZfs $
--             newDescribeLocationFsxOpenZfs
--
--         , requestDescribeLocationFsxWindows $
--             newDescribeLocationFsxWindows
--
--         , requestDescribeLocationHdfs $
--             newDescribeLocationHdfs
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
--         , requestDescribeStorageSystem $
--             newDescribeStorageSystem
--
--         , requestDescribeStorageSystemResourceMetrics $
--             newDescribeStorageSystemResourceMetrics
--
--         , requestDescribeStorageSystemResources $
--             newDescribeStorageSystemResources
--
--         , requestDescribeTask $
--             newDescribeTask
--
--         , requestDescribeTaskExecution $
--             newDescribeTaskExecution
--
--         , requestGenerateRecommendations $
--             newGenerateRecommendations
--
--         , requestListAgents $
--             newListAgents
--
--         , requestListDiscoveryJobs $
--             newListDiscoveryJobs
--
--         , requestListLocations $
--             newListLocations
--
--         , requestListStorageSystems $
--             newListStorageSystems
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
--         , requestRemoveStorageSystem $
--             newRemoveStorageSystem
--
--         , requestStartDiscoveryJob $
--             newStartDiscoveryJob
--
--         , requestStartTaskExecution $
--             newStartTaskExecution
--
--         , requestStopDiscoveryJob $
--             newStopDiscoveryJob
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
--         , requestUpdateDiscoveryJob $
--             newUpdateDiscoveryJob
--
--         , requestUpdateLocationHdfs $
--             newUpdateLocationHdfs
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
--         , requestUpdateStorageSystem $
--             newUpdateStorageSystem
--
--         , requestUpdateTask $
--             newUpdateTask
--
--         , requestUpdateTaskExecution $
--             newUpdateTaskExecution
--
--           ]

--     , testGroup "response"
--         [ responseAddStorageSystem $
--             newAddStorageSystemResponse
--
--         , responseCancelTaskExecution $
--             newCancelTaskExecutionResponse
--
--         , responseCreateAgent $
--             newCreateAgentResponse
--
--         , responseCreateLocationEfs $
--             newCreateLocationEfsResponse
--
--         , responseCreateLocationFsxLustre $
--             newCreateLocationFsxLustreResponse
--
--         , responseCreateLocationFsxOntap $
--             newCreateLocationFsxOntapResponse
--
--         , responseCreateLocationFsxOpenZfs $
--             newCreateLocationFsxOpenZfsResponse
--
--         , responseCreateLocationFsxWindows $
--             newCreateLocationFsxWindowsResponse
--
--         , responseCreateLocationHdfs $
--             newCreateLocationHdfsResponse
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
--         , responseDescribeDiscoveryJob $
--             newDescribeDiscoveryJobResponse
--
--         , responseDescribeLocationEfs $
--             newDescribeLocationEfsResponse
--
--         , responseDescribeLocationFsxLustre $
--             newDescribeLocationFsxLustreResponse
--
--         , responseDescribeLocationFsxOntap $
--             newDescribeLocationFsxOntapResponse
--
--         , responseDescribeLocationFsxOpenZfs $
--             newDescribeLocationFsxOpenZfsResponse
--
--         , responseDescribeLocationFsxWindows $
--             newDescribeLocationFsxWindowsResponse
--
--         , responseDescribeLocationHdfs $
--             newDescribeLocationHdfsResponse
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
--         , responseDescribeStorageSystem $
--             newDescribeStorageSystemResponse
--
--         , responseDescribeStorageSystemResourceMetrics $
--             newDescribeStorageSystemResourceMetricsResponse
--
--         , responseDescribeStorageSystemResources $
--             newDescribeStorageSystemResourcesResponse
--
--         , responseDescribeTask $
--             newDescribeTaskResponse
--
--         , responseDescribeTaskExecution $
--             newDescribeTaskExecutionResponse
--
--         , responseGenerateRecommendations $
--             newGenerateRecommendationsResponse
--
--         , responseListAgents $
--             newListAgentsResponse
--
--         , responseListDiscoveryJobs $
--             newListDiscoveryJobsResponse
--
--         , responseListLocations $
--             newListLocationsResponse
--
--         , responseListStorageSystems $
--             newListStorageSystemsResponse
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
--         , responseRemoveStorageSystem $
--             newRemoveStorageSystemResponse
--
--         , responseStartDiscoveryJob $
--             newStartDiscoveryJobResponse
--
--         , responseStartTaskExecution $
--             newStartTaskExecutionResponse
--
--         , responseStopDiscoveryJob $
--             newStopDiscoveryJobResponse
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
--         , responseUpdateDiscoveryJob $
--             newUpdateDiscoveryJobResponse
--
--         , responseUpdateLocationHdfs $
--             newUpdateLocationHdfsResponse
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
--         , responseUpdateStorageSystem $
--             newUpdateStorageSystemResponse
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

requestAddStorageSystem :: AddStorageSystem -> TestTree
requestAddStorageSystem =
  req
    "AddStorageSystem"
    "fixture/AddStorageSystem.yaml"

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

requestCreateLocationFsxLustre :: CreateLocationFsxLustre -> TestTree
requestCreateLocationFsxLustre =
  req
    "CreateLocationFsxLustre"
    "fixture/CreateLocationFsxLustre.yaml"

requestCreateLocationFsxOntap :: CreateLocationFsxOntap -> TestTree
requestCreateLocationFsxOntap =
  req
    "CreateLocationFsxOntap"
    "fixture/CreateLocationFsxOntap.yaml"

requestCreateLocationFsxOpenZfs :: CreateLocationFsxOpenZfs -> TestTree
requestCreateLocationFsxOpenZfs =
  req
    "CreateLocationFsxOpenZfs"
    "fixture/CreateLocationFsxOpenZfs.yaml"

requestCreateLocationFsxWindows :: CreateLocationFsxWindows -> TestTree
requestCreateLocationFsxWindows =
  req
    "CreateLocationFsxWindows"
    "fixture/CreateLocationFsxWindows.yaml"

requestCreateLocationHdfs :: CreateLocationHdfs -> TestTree
requestCreateLocationHdfs =
  req
    "CreateLocationHdfs"
    "fixture/CreateLocationHdfs.yaml"

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

requestDescribeDiscoveryJob :: DescribeDiscoveryJob -> TestTree
requestDescribeDiscoveryJob =
  req
    "DescribeDiscoveryJob"
    "fixture/DescribeDiscoveryJob.yaml"

requestDescribeLocationEfs :: DescribeLocationEfs -> TestTree
requestDescribeLocationEfs =
  req
    "DescribeLocationEfs"
    "fixture/DescribeLocationEfs.yaml"

requestDescribeLocationFsxLustre :: DescribeLocationFsxLustre -> TestTree
requestDescribeLocationFsxLustre =
  req
    "DescribeLocationFsxLustre"
    "fixture/DescribeLocationFsxLustre.yaml"

requestDescribeLocationFsxOntap :: DescribeLocationFsxOntap -> TestTree
requestDescribeLocationFsxOntap =
  req
    "DescribeLocationFsxOntap"
    "fixture/DescribeLocationFsxOntap.yaml"

requestDescribeLocationFsxOpenZfs :: DescribeLocationFsxOpenZfs -> TestTree
requestDescribeLocationFsxOpenZfs =
  req
    "DescribeLocationFsxOpenZfs"
    "fixture/DescribeLocationFsxOpenZfs.yaml"

requestDescribeLocationFsxWindows :: DescribeLocationFsxWindows -> TestTree
requestDescribeLocationFsxWindows =
  req
    "DescribeLocationFsxWindows"
    "fixture/DescribeLocationFsxWindows.yaml"

requestDescribeLocationHdfs :: DescribeLocationHdfs -> TestTree
requestDescribeLocationHdfs =
  req
    "DescribeLocationHdfs"
    "fixture/DescribeLocationHdfs.yaml"

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

requestDescribeStorageSystem :: DescribeStorageSystem -> TestTree
requestDescribeStorageSystem =
  req
    "DescribeStorageSystem"
    "fixture/DescribeStorageSystem.yaml"

requestDescribeStorageSystemResourceMetrics :: DescribeStorageSystemResourceMetrics -> TestTree
requestDescribeStorageSystemResourceMetrics =
  req
    "DescribeStorageSystemResourceMetrics"
    "fixture/DescribeStorageSystemResourceMetrics.yaml"

requestDescribeStorageSystemResources :: DescribeStorageSystemResources -> TestTree
requestDescribeStorageSystemResources =
  req
    "DescribeStorageSystemResources"
    "fixture/DescribeStorageSystemResources.yaml"

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

requestGenerateRecommendations :: GenerateRecommendations -> TestTree
requestGenerateRecommendations =
  req
    "GenerateRecommendations"
    "fixture/GenerateRecommendations.yaml"

requestListAgents :: ListAgents -> TestTree
requestListAgents =
  req
    "ListAgents"
    "fixture/ListAgents.yaml"

requestListDiscoveryJobs :: ListDiscoveryJobs -> TestTree
requestListDiscoveryJobs =
  req
    "ListDiscoveryJobs"
    "fixture/ListDiscoveryJobs.yaml"

requestListLocations :: ListLocations -> TestTree
requestListLocations =
  req
    "ListLocations"
    "fixture/ListLocations.yaml"

requestListStorageSystems :: ListStorageSystems -> TestTree
requestListStorageSystems =
  req
    "ListStorageSystems"
    "fixture/ListStorageSystems.yaml"

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

requestRemoveStorageSystem :: RemoveStorageSystem -> TestTree
requestRemoveStorageSystem =
  req
    "RemoveStorageSystem"
    "fixture/RemoveStorageSystem.yaml"

requestStartDiscoveryJob :: StartDiscoveryJob -> TestTree
requestStartDiscoveryJob =
  req
    "StartDiscoveryJob"
    "fixture/StartDiscoveryJob.yaml"

requestStartTaskExecution :: StartTaskExecution -> TestTree
requestStartTaskExecution =
  req
    "StartTaskExecution"
    "fixture/StartTaskExecution.yaml"

requestStopDiscoveryJob :: StopDiscoveryJob -> TestTree
requestStopDiscoveryJob =
  req
    "StopDiscoveryJob"
    "fixture/StopDiscoveryJob.yaml"

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

requestUpdateDiscoveryJob :: UpdateDiscoveryJob -> TestTree
requestUpdateDiscoveryJob =
  req
    "UpdateDiscoveryJob"
    "fixture/UpdateDiscoveryJob.yaml"

requestUpdateLocationHdfs :: UpdateLocationHdfs -> TestTree
requestUpdateLocationHdfs =
  req
    "UpdateLocationHdfs"
    "fixture/UpdateLocationHdfs.yaml"

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

requestUpdateStorageSystem :: UpdateStorageSystem -> TestTree
requestUpdateStorageSystem =
  req
    "UpdateStorageSystem"
    "fixture/UpdateStorageSystem.yaml"

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

responseAddStorageSystem :: AddStorageSystemResponse -> TestTree
responseAddStorageSystem =
  res
    "AddStorageSystemResponse"
    "fixture/AddStorageSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddStorageSystem)

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

responseCreateLocationFsxLustre :: CreateLocationFsxLustreResponse -> TestTree
responseCreateLocationFsxLustre =
  res
    "CreateLocationFsxLustreResponse"
    "fixture/CreateLocationFsxLustreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationFsxLustre)

responseCreateLocationFsxOntap :: CreateLocationFsxOntapResponse -> TestTree
responseCreateLocationFsxOntap =
  res
    "CreateLocationFsxOntapResponse"
    "fixture/CreateLocationFsxOntapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationFsxOntap)

responseCreateLocationFsxOpenZfs :: CreateLocationFsxOpenZfsResponse -> TestTree
responseCreateLocationFsxOpenZfs =
  res
    "CreateLocationFsxOpenZfsResponse"
    "fixture/CreateLocationFsxOpenZfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationFsxOpenZfs)

responseCreateLocationFsxWindows :: CreateLocationFsxWindowsResponse -> TestTree
responseCreateLocationFsxWindows =
  res
    "CreateLocationFsxWindowsResponse"
    "fixture/CreateLocationFsxWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationFsxWindows)

responseCreateLocationHdfs :: CreateLocationHdfsResponse -> TestTree
responseCreateLocationHdfs =
  res
    "CreateLocationHdfsResponse"
    "fixture/CreateLocationHdfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLocationHdfs)

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

responseDescribeDiscoveryJob :: DescribeDiscoveryJobResponse -> TestTree
responseDescribeDiscoveryJob =
  res
    "DescribeDiscoveryJobResponse"
    "fixture/DescribeDiscoveryJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDiscoveryJob)

responseDescribeLocationEfs :: DescribeLocationEfsResponse -> TestTree
responseDescribeLocationEfs =
  res
    "DescribeLocationEfsResponse"
    "fixture/DescribeLocationEfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationEfs)

responseDescribeLocationFsxLustre :: DescribeLocationFsxLustreResponse -> TestTree
responseDescribeLocationFsxLustre =
  res
    "DescribeLocationFsxLustreResponse"
    "fixture/DescribeLocationFsxLustreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationFsxLustre)

responseDescribeLocationFsxOntap :: DescribeLocationFsxOntapResponse -> TestTree
responseDescribeLocationFsxOntap =
  res
    "DescribeLocationFsxOntapResponse"
    "fixture/DescribeLocationFsxOntapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationFsxOntap)

responseDescribeLocationFsxOpenZfs :: DescribeLocationFsxOpenZfsResponse -> TestTree
responseDescribeLocationFsxOpenZfs =
  res
    "DescribeLocationFsxOpenZfsResponse"
    "fixture/DescribeLocationFsxOpenZfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationFsxOpenZfs)

responseDescribeLocationFsxWindows :: DescribeLocationFsxWindowsResponse -> TestTree
responseDescribeLocationFsxWindows =
  res
    "DescribeLocationFsxWindowsResponse"
    "fixture/DescribeLocationFsxWindowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationFsxWindows)

responseDescribeLocationHdfs :: DescribeLocationHdfsResponse -> TestTree
responseDescribeLocationHdfs =
  res
    "DescribeLocationHdfsResponse"
    "fixture/DescribeLocationHdfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLocationHdfs)

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

responseDescribeStorageSystem :: DescribeStorageSystemResponse -> TestTree
responseDescribeStorageSystem =
  res
    "DescribeStorageSystemResponse"
    "fixture/DescribeStorageSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorageSystem)

responseDescribeStorageSystemResourceMetrics :: DescribeStorageSystemResourceMetricsResponse -> TestTree
responseDescribeStorageSystemResourceMetrics =
  res
    "DescribeStorageSystemResourceMetricsResponse"
    "fixture/DescribeStorageSystemResourceMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorageSystemResourceMetrics)

responseDescribeStorageSystemResources :: DescribeStorageSystemResourcesResponse -> TestTree
responseDescribeStorageSystemResources =
  res
    "DescribeStorageSystemResourcesResponse"
    "fixture/DescribeStorageSystemResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorageSystemResources)

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

responseGenerateRecommendations :: GenerateRecommendationsResponse -> TestTree
responseGenerateRecommendations =
  res
    "GenerateRecommendationsResponse"
    "fixture/GenerateRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateRecommendations)

responseListAgents :: ListAgentsResponse -> TestTree
responseListAgents =
  res
    "ListAgentsResponse"
    "fixture/ListAgentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAgents)

responseListDiscoveryJobs :: ListDiscoveryJobsResponse -> TestTree
responseListDiscoveryJobs =
  res
    "ListDiscoveryJobsResponse"
    "fixture/ListDiscoveryJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDiscoveryJobs)

responseListLocations :: ListLocationsResponse -> TestTree
responseListLocations =
  res
    "ListLocationsResponse"
    "fixture/ListLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLocations)

responseListStorageSystems :: ListStorageSystemsResponse -> TestTree
responseListStorageSystems =
  res
    "ListStorageSystemsResponse"
    "fixture/ListStorageSystemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStorageSystems)

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

responseRemoveStorageSystem :: RemoveStorageSystemResponse -> TestTree
responseRemoveStorageSystem =
  res
    "RemoveStorageSystemResponse"
    "fixture/RemoveStorageSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveStorageSystem)

responseStartDiscoveryJob :: StartDiscoveryJobResponse -> TestTree
responseStartDiscoveryJob =
  res
    "StartDiscoveryJobResponse"
    "fixture/StartDiscoveryJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDiscoveryJob)

responseStartTaskExecution :: StartTaskExecutionResponse -> TestTree
responseStartTaskExecution =
  res
    "StartTaskExecutionResponse"
    "fixture/StartTaskExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTaskExecution)

responseStopDiscoveryJob :: StopDiscoveryJobResponse -> TestTree
responseStopDiscoveryJob =
  res
    "StopDiscoveryJobResponse"
    "fixture/StopDiscoveryJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDiscoveryJob)

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

responseUpdateDiscoveryJob :: UpdateDiscoveryJobResponse -> TestTree
responseUpdateDiscoveryJob =
  res
    "UpdateDiscoveryJobResponse"
    "fixture/UpdateDiscoveryJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDiscoveryJob)

responseUpdateLocationHdfs :: UpdateLocationHdfsResponse -> TestTree
responseUpdateLocationHdfs =
  res
    "UpdateLocationHdfsResponse"
    "fixture/UpdateLocationHdfsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLocationHdfs)

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

responseUpdateStorageSystem :: UpdateStorageSystemResponse -> TestTree
responseUpdateStorageSystem =
  res
    "UpdateStorageSystemResponse"
    "fixture/UpdateStorageSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStorageSystem)

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
