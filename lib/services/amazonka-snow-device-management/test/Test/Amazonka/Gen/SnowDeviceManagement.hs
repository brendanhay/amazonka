{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SnowDeviceManagement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SnowDeviceManagement where

import Amazonka.SnowDeviceManagement
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SnowDeviceManagement.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelTask $
--             newCancelTask
--
--         , requestCreateTask $
--             newCreateTask
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestDescribeDeviceEc2Instances $
--             newDescribeDeviceEc2Instances
--
--         , requestDescribeExecution $
--             newDescribeExecution
--
--         , requestDescribeTask $
--             newDescribeTask
--
--         , requestListDeviceResources $
--             newListDeviceResources
--
--         , requestListDevices $
--             newListDevices
--
--         , requestListExecutions $
--             newListExecutions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTasks $
--             newListTasks
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCancelTask $
--             newCancelTaskResponse
--
--         , responseCreateTask $
--             newCreateTaskResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseDescribeDeviceEc2Instances $
--             newDescribeDeviceEc2InstancesResponse
--
--         , responseDescribeExecution $
--             newDescribeExecutionResponse
--
--         , responseDescribeTask $
--             newDescribeTaskResponse
--
--         , responseListDeviceResources $
--             newListDeviceResourcesResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseListExecutions $
--             newListExecutionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTasks $
--             newListTasksResponse
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

requestCancelTask :: CancelTask -> TestTree
requestCancelTask =
  req
    "CancelTask"
    "fixture/CancelTask.yaml"

requestCreateTask :: CreateTask -> TestTree
requestCreateTask =
  req
    "CreateTask"
    "fixture/CreateTask.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestDescribeDeviceEc2Instances :: DescribeDeviceEc2Instances -> TestTree
requestDescribeDeviceEc2Instances =
  req
    "DescribeDeviceEc2Instances"
    "fixture/DescribeDeviceEc2Instances.yaml"

requestDescribeExecution :: DescribeExecution -> TestTree
requestDescribeExecution =
  req
    "DescribeExecution"
    "fixture/DescribeExecution.yaml"

requestDescribeTask :: DescribeTask -> TestTree
requestDescribeTask =
  req
    "DescribeTask"
    "fixture/DescribeTask.yaml"

requestListDeviceResources :: ListDeviceResources -> TestTree
requestListDeviceResources =
  req
    "ListDeviceResources"
    "fixture/ListDeviceResources.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestListExecutions :: ListExecutions -> TestTree
requestListExecutions =
  req
    "ListExecutions"
    "fixture/ListExecutions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTasks :: ListTasks -> TestTree
requestListTasks =
  req
    "ListTasks"
    "fixture/ListTasks.yaml"

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

responseCancelTask :: CancelTaskResponse -> TestTree
responseCancelTask =
  res
    "CancelTaskResponse"
    "fixture/CancelTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelTask)

responseCreateTask :: CreateTaskResponse -> TestTree
responseCreateTask =
  res
    "CreateTaskResponse"
    "fixture/CreateTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTask)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevice)

responseDescribeDeviceEc2Instances :: DescribeDeviceEc2InstancesResponse -> TestTree
responseDescribeDeviceEc2Instances =
  res
    "DescribeDeviceEc2InstancesResponse"
    "fixture/DescribeDeviceEc2InstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeviceEc2Instances)

responseDescribeExecution :: DescribeExecutionResponse -> TestTree
responseDescribeExecution =
  res
    "DescribeExecutionResponse"
    "fixture/DescribeExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExecution)

responseDescribeTask :: DescribeTaskResponse -> TestTree
responseDescribeTask =
  res
    "DescribeTaskResponse"
    "fixture/DescribeTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTask)

responseListDeviceResources :: ListDeviceResourcesResponse -> TestTree
responseListDeviceResources =
  res
    "ListDeviceResourcesResponse"
    "fixture/ListDeviceResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceResources)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)

responseListExecutions :: ListExecutionsResponse -> TestTree
responseListExecutions =
  res
    "ListExecutionsResponse"
    "fixture/ListExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExecutions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks =
  res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTasks)

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
