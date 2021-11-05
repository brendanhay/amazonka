{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SnowDeviceManagement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SnowDeviceManagement where

import qualified Data.Proxy as Proxy
import Network.AWS.SnowDeviceManagement
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SnowDeviceManagement.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeTask $
--             newDescribeTask
--
--         , requestListDeviceResources $
--             newListDeviceResources
--
--         , requestListExecutions $
--             newListExecutions
--
--         , requestDescribeDeviceEc2Instances $
--             newDescribeDeviceEc2Instances
--
--         , requestCreateTask $
--             newCreateTask
--
--         , requestListTasks $
--             newListTasks
--
--         , requestDescribeExecution $
--             newDescribeExecution
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCancelTask $
--             newCancelTask
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListDevices $
--             newListDevices
--
--           ]

--     , testGroup "response"
--         [ responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeTask $
--             newDescribeTaskResponse
--
--         , responseListDeviceResources $
--             newListDeviceResourcesResponse
--
--         , responseListExecutions $
--             newListExecutionsResponse
--
--         , responseDescribeDeviceEc2Instances $
--             newDescribeDeviceEc2InstancesResponse
--
--         , responseCreateTask $
--             newCreateTaskResponse
--
--         , responseListTasks $
--             newListTasksResponse
--
--         , responseDescribeExecution $
--             newDescribeExecutionResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCancelTask $
--             newCancelTaskResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--           ]
--     ]

-- Requests

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestListExecutions :: ListExecutions -> TestTree
requestListExecutions =
  req
    "ListExecutions"
    "fixture/ListExecutions.yaml"

requestDescribeDeviceEc2Instances :: DescribeDeviceEc2Instances -> TestTree
requestDescribeDeviceEc2Instances =
  req
    "DescribeDeviceEc2Instances"
    "fixture/DescribeDeviceEc2Instances.yaml"

requestCreateTask :: CreateTask -> TestTree
requestCreateTask =
  req
    "CreateTask"
    "fixture/CreateTask.yaml"

requestListTasks :: ListTasks -> TestTree
requestListTasks =
  req
    "ListTasks"
    "fixture/ListTasks.yaml"

requestDescribeExecution :: DescribeExecution -> TestTree
requestDescribeExecution =
  req
    "DescribeExecution"
    "fixture/DescribeExecution.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCancelTask :: CancelTask -> TestTree
requestCancelTask =
  req
    "CancelTask"
    "fixture/CancelTask.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

-- Responses

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseListExecutions :: ListExecutionsResponse -> TestTree
responseListExecutions =
  res
    "ListExecutionsResponse"
    "fixture/ListExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExecutions)

responseDescribeDeviceEc2Instances :: DescribeDeviceEc2InstancesResponse -> TestTree
responseDescribeDeviceEc2Instances =
  res
    "DescribeDeviceEc2InstancesResponse"
    "fixture/DescribeDeviceEc2InstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeviceEc2Instances)

responseCreateTask :: CreateTaskResponse -> TestTree
responseCreateTask =
  res
    "CreateTaskResponse"
    "fixture/CreateTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTask)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks =
  res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTasks)

responseDescribeExecution :: DescribeExecutionResponse -> TestTree
responseDescribeExecution =
  res
    "DescribeExecutionResponse"
    "fixture/DescribeExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExecution)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevice)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCancelTask :: CancelTaskResponse -> TestTree
responseCancelTask =
  res
    "CancelTaskResponse"
    "fixture/CancelTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelTask)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)
