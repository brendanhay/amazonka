{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Braket
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Braket where

import Amazonka.Braket
import qualified Data.Proxy as Proxy
import Test.Amazonka.Braket.Internal
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
--         [ requestCancelJob $
--             newCancelJob
--
--         , requestCancelQuantumTask $
--             newCancelQuantumTask
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestCreateQuantumTask $
--             newCreateQuantumTask
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestGetJob $
--             newGetJob
--
--         , requestGetQuantumTask $
--             newGetQuantumTask
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSearchDevices $
--             newSearchDevices
--
--         , requestSearchJobs $
--             newSearchJobs
--
--         , requestSearchQuantumTasks $
--             newSearchQuantumTasks
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCancelJob $
--             newCancelJobResponse
--
--         , responseCancelQuantumTask $
--             newCancelQuantumTaskResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseCreateQuantumTask $
--             newCreateQuantumTaskResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseGetQuantumTask $
--             newGetQuantumTaskResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSearchDevices $
--             newSearchDevicesResponse
--
--         , responseSearchJobs $
--             newSearchJobsResponse
--
--         , responseSearchQuantumTasks $
--             newSearchQuantumTasksResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestCancelQuantumTask :: CancelQuantumTask -> TestTree
requestCancelQuantumTask =
  req
    "CancelQuantumTask"
    "fixture/CancelQuantumTask.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestCreateQuantumTask :: CreateQuantumTask -> TestTree
requestCreateQuantumTask =
  req
    "CreateQuantumTask"
    "fixture/CreateQuantumTask.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestGetQuantumTask :: GetQuantumTask -> TestTree
requestGetQuantumTask =
  req
    "GetQuantumTask"
    "fixture/GetQuantumTask.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSearchDevices :: SearchDevices -> TestTree
requestSearchDevices =
  req
    "SearchDevices"
    "fixture/SearchDevices.yaml"

requestSearchJobs :: SearchJobs -> TestTree
requestSearchJobs =
  req
    "SearchJobs"
    "fixture/SearchJobs.yaml"

requestSearchQuantumTasks :: SearchQuantumTasks -> TestTree
requestSearchQuantumTasks =
  req
    "SearchQuantumTasks"
    "fixture/SearchQuantumTasks.yaml"

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

-- Responses

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)

responseCancelQuantumTask :: CancelQuantumTaskResponse -> TestTree
responseCancelQuantumTask =
  res
    "CancelQuantumTaskResponse"
    "fixture/CancelQuantumTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelQuantumTask)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseCreateQuantumTask :: CreateQuantumTaskResponse -> TestTree
responseCreateQuantumTask =
  res
    "CreateQuantumTaskResponse"
    "fixture/CreateQuantumTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQuantumTask)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevice)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseGetQuantumTask :: GetQuantumTaskResponse -> TestTree
responseGetQuantumTask =
  res
    "GetQuantumTaskResponse"
    "fixture/GetQuantumTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQuantumTask)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseSearchDevices :: SearchDevicesResponse -> TestTree
responseSearchDevices =
  res
    "SearchDevicesResponse"
    "fixture/SearchDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchDevices)

responseSearchJobs :: SearchJobsResponse -> TestTree
responseSearchJobs =
  res
    "SearchJobsResponse"
    "fixture/SearchJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchJobs)

responseSearchQuantumTasks :: SearchQuantumTasksResponse -> TestTree
responseSearchQuantumTasks =
  res
    "SearchQuantumTasksResponse"
    "fixture/SearchQuantumTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchQuantumTasks)

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
