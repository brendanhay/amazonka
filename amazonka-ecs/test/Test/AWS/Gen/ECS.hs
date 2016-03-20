{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ECS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ECS
import Test.AWS.ECS.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testListServices $
--             listServices
--
--         , testDescribeClusters $
--             describeClusters
--
--         , testDeleteService $
--             deleteService
--
--         , testUpdateService $
--             updateService
--
--         , testDiscoverPollEndpoint $
--             discoverPollEndpoint
--
--         , testSubmitContainerStateChange $
--             submitContainerStateChange
--
--         , testStopTask $
--             stopTask
--
--         , testDescribeTaskDefinition $
--             describeTaskDefinition
--
--         , testSubmitTaskStateChange $
--             submitTaskStateChange
--
--         , testDescribeContainerInstances $
--             describeContainerInstances
--
--         , testDeleteCluster $
--             deleteCluster
--
--         , testCreateCluster $
--             createCluster
--
--         , testListTaskDefinitions $
--             listTaskDefinitions
--
--         , testRunTask $
--             runTask
--
--         , testListTasks $
--             listTasks
--
--         , testRegisterContainerInstance $
--             registerContainerInstance
--
--         , testUpdateContainerAgent $
--             updateContainerAgent
--
--         , testListContainerInstances $
--             listContainerInstances
--
--         , testListTaskDefinitionFamilies $
--             listTaskDefinitionFamilies
--
--         , testStartTask $
--             startTask
--
--         , testDeregisterTaskDefinition $
--             deregisterTaskDefinition
--
--         , testDescribeTasks $
--             describeTasks
--
--         , testListClusters $
--             listClusters
--
--         , testDescribeServices $
--             describeServices
--
--         , testDeregisterContainerInstance $
--             deregisterContainerInstance
--
--         , testRegisterTaskDefinition $
--             registerTaskDefinition
--
--         , testCreateService $
--             createService
--
--           ]

--     , testGroup "response"
--         [ testListServicesResponse $
--             listServicesResponse
--
--         , testDescribeClustersResponse $
--             describeClustersResponse
--
--         , testDeleteServiceResponse $
--             deleteServiceResponse
--
--         , testUpdateServiceResponse $
--             updateServiceResponse
--
--         , testDiscoverPollEndpointResponse $
--             discoverPollEndpointResponse
--
--         , testSubmitContainerStateChangeResponse $
--             submitContainerStateChangeResponse
--
--         , testStopTaskResponse $
--             stopTaskResponse
--
--         , testDescribeTaskDefinitionResponse $
--             describeTaskDefinitionResponse
--
--         , testSubmitTaskStateChangeResponse $
--             submitTaskStateChangeResponse
--
--         , testDescribeContainerInstancesResponse $
--             describeContainerInstancesResponse
--
--         , testDeleteClusterResponse $
--             deleteClusterResponse
--
--         , testCreateClusterResponse $
--             createClusterResponse
--
--         , testListTaskDefinitionsResponse $
--             listTaskDefinitionsResponse
--
--         , testRunTaskResponse $
--             runTaskResponse
--
--         , testListTasksResponse $
--             listTasksResponse
--
--         , testRegisterContainerInstanceResponse $
--             registerContainerInstanceResponse
--
--         , testUpdateContainerAgentResponse $
--             updateContainerAgentResponse
--
--         , testListContainerInstancesResponse $
--             listContainerInstancesResponse
--
--         , testListTaskDefinitionFamiliesResponse $
--             listTaskDefinitionFamiliesResponse
--
--         , testStartTaskResponse $
--             startTaskResponse
--
--         , testDeregisterTaskDefinitionResponse $
--             deregisterTaskDefinitionResponse
--
--         , testDescribeTasksResponse $
--             describeTasksResponse
--
--         , testListClustersResponse $
--             listClustersResponse
--
--         , testDescribeServicesResponse $
--             describeServicesResponse
--
--         , testDeregisterContainerInstanceResponse $
--             deregisterContainerInstanceResponse
--
--         , testRegisterTaskDefinitionResponse $
--             registerTaskDefinitionResponse
--
--         , testCreateServiceResponse $
--             createServiceResponse
--
--           ]
--     ]

-- Requests

testListServices :: ListServices -> TestTree
testListServices = req
    "ListServices"
    "fixture/ListServices.yaml"

testDescribeClusters :: DescribeClusters -> TestTree
testDescribeClusters = req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

testDeleteService :: DeleteService -> TestTree
testDeleteService = req
    "DeleteService"
    "fixture/DeleteService.yaml"

testUpdateService :: UpdateService -> TestTree
testUpdateService = req
    "UpdateService"
    "fixture/UpdateService.yaml"

testDiscoverPollEndpoint :: DiscoverPollEndpoint -> TestTree
testDiscoverPollEndpoint = req
    "DiscoverPollEndpoint"
    "fixture/DiscoverPollEndpoint.yaml"

testSubmitContainerStateChange :: SubmitContainerStateChange -> TestTree
testSubmitContainerStateChange = req
    "SubmitContainerStateChange"
    "fixture/SubmitContainerStateChange.yaml"

testStopTask :: StopTask -> TestTree
testStopTask = req
    "StopTask"
    "fixture/StopTask.yaml"

testDescribeTaskDefinition :: DescribeTaskDefinition -> TestTree
testDescribeTaskDefinition = req
    "DescribeTaskDefinition"
    "fixture/DescribeTaskDefinition.yaml"

testSubmitTaskStateChange :: SubmitTaskStateChange -> TestTree
testSubmitTaskStateChange = req
    "SubmitTaskStateChange"
    "fixture/SubmitTaskStateChange.yaml"

testDescribeContainerInstances :: DescribeContainerInstances -> TestTree
testDescribeContainerInstances = req
    "DescribeContainerInstances"
    "fixture/DescribeContainerInstances.yaml"

testDeleteCluster :: DeleteCluster -> TestTree
testDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

testCreateCluster :: CreateCluster -> TestTree
testCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

testListTaskDefinitions :: ListTaskDefinitions -> TestTree
testListTaskDefinitions = req
    "ListTaskDefinitions"
    "fixture/ListTaskDefinitions.yaml"

testRunTask :: RunTask -> TestTree
testRunTask = req
    "RunTask"
    "fixture/RunTask.yaml"

testListTasks :: ListTasks -> TestTree
testListTasks = req
    "ListTasks"
    "fixture/ListTasks.yaml"

testRegisterContainerInstance :: RegisterContainerInstance -> TestTree
testRegisterContainerInstance = req
    "RegisterContainerInstance"
    "fixture/RegisterContainerInstance.yaml"

testUpdateContainerAgent :: UpdateContainerAgent -> TestTree
testUpdateContainerAgent = req
    "UpdateContainerAgent"
    "fixture/UpdateContainerAgent.yaml"

testListContainerInstances :: ListContainerInstances -> TestTree
testListContainerInstances = req
    "ListContainerInstances"
    "fixture/ListContainerInstances.yaml"

testListTaskDefinitionFamilies :: ListTaskDefinitionFamilies -> TestTree
testListTaskDefinitionFamilies = req
    "ListTaskDefinitionFamilies"
    "fixture/ListTaskDefinitionFamilies.yaml"

testStartTask :: StartTask -> TestTree
testStartTask = req
    "StartTask"
    "fixture/StartTask.yaml"

testDeregisterTaskDefinition :: DeregisterTaskDefinition -> TestTree
testDeregisterTaskDefinition = req
    "DeregisterTaskDefinition"
    "fixture/DeregisterTaskDefinition.yaml"

testDescribeTasks :: DescribeTasks -> TestTree
testDescribeTasks = req
    "DescribeTasks"
    "fixture/DescribeTasks.yaml"

testListClusters :: ListClusters -> TestTree
testListClusters = req
    "ListClusters"
    "fixture/ListClusters.yaml"

testDescribeServices :: DescribeServices -> TestTree
testDescribeServices = req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

testDeregisterContainerInstance :: DeregisterContainerInstance -> TestTree
testDeregisterContainerInstance = req
    "DeregisterContainerInstance"
    "fixture/DeregisterContainerInstance.yaml"

testRegisterTaskDefinition :: RegisterTaskDefinition -> TestTree
testRegisterTaskDefinition = req
    "RegisterTaskDefinition"
    "fixture/RegisterTaskDefinition.yaml"

testCreateService :: CreateService -> TestTree
testCreateService = req
    "CreateService"
    "fixture/CreateService.yaml"

-- Responses

testListServicesResponse :: ListServicesResponse -> TestTree
testListServicesResponse = res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    ecs
    (Proxy :: Proxy ListServices)

testDescribeClustersResponse :: DescribeClustersResponse -> TestTree
testDescribeClustersResponse = res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    ecs
    (Proxy :: Proxy DescribeClusters)

testDeleteServiceResponse :: DeleteServiceResponse -> TestTree
testDeleteServiceResponse = res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    ecs
    (Proxy :: Proxy DeleteService)

testUpdateServiceResponse :: UpdateServiceResponse -> TestTree
testUpdateServiceResponse = res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    ecs
    (Proxy :: Proxy UpdateService)

testDiscoverPollEndpointResponse :: DiscoverPollEndpointResponse -> TestTree
testDiscoverPollEndpointResponse = res
    "DiscoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse.proto"
    ecs
    (Proxy :: Proxy DiscoverPollEndpoint)

testSubmitContainerStateChangeResponse :: SubmitContainerStateChangeResponse -> TestTree
testSubmitContainerStateChangeResponse = res
    "SubmitContainerStateChangeResponse"
    "fixture/SubmitContainerStateChangeResponse.proto"
    ecs
    (Proxy :: Proxy SubmitContainerStateChange)

testStopTaskResponse :: StopTaskResponse -> TestTree
testStopTaskResponse = res
    "StopTaskResponse"
    "fixture/StopTaskResponse.proto"
    ecs
    (Proxy :: Proxy StopTask)

testDescribeTaskDefinitionResponse :: DescribeTaskDefinitionResponse -> TestTree
testDescribeTaskDefinitionResponse = res
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse.proto"
    ecs
    (Proxy :: Proxy DescribeTaskDefinition)

testSubmitTaskStateChangeResponse :: SubmitTaskStateChangeResponse -> TestTree
testSubmitTaskStateChangeResponse = res
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse.proto"
    ecs
    (Proxy :: Proxy SubmitTaskStateChange)

testDescribeContainerInstancesResponse :: DescribeContainerInstancesResponse -> TestTree
testDescribeContainerInstancesResponse = res
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse.proto"
    ecs
    (Proxy :: Proxy DescribeContainerInstances)

testDeleteClusterResponse :: DeleteClusterResponse -> TestTree
testDeleteClusterResponse = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    ecs
    (Proxy :: Proxy DeleteCluster)

testCreateClusterResponse :: CreateClusterResponse -> TestTree
testCreateClusterResponse = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    ecs
    (Proxy :: Proxy CreateCluster)

testListTaskDefinitionsResponse :: ListTaskDefinitionsResponse -> TestTree
testListTaskDefinitionsResponse = res
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse.proto"
    ecs
    (Proxy :: Proxy ListTaskDefinitions)

testRunTaskResponse :: RunTaskResponse -> TestTree
testRunTaskResponse = res
    "RunTaskResponse"
    "fixture/RunTaskResponse.proto"
    ecs
    (Proxy :: Proxy RunTask)

testListTasksResponse :: ListTasksResponse -> TestTree
testListTasksResponse = res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    ecs
    (Proxy :: Proxy ListTasks)

testRegisterContainerInstanceResponse :: RegisterContainerInstanceResponse -> TestTree
testRegisterContainerInstanceResponse = res
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse.proto"
    ecs
    (Proxy :: Proxy RegisterContainerInstance)

testUpdateContainerAgentResponse :: UpdateContainerAgentResponse -> TestTree
testUpdateContainerAgentResponse = res
    "UpdateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse.proto"
    ecs
    (Proxy :: Proxy UpdateContainerAgent)

testListContainerInstancesResponse :: ListContainerInstancesResponse -> TestTree
testListContainerInstancesResponse = res
    "ListContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse.proto"
    ecs
    (Proxy :: Proxy ListContainerInstances)

testListTaskDefinitionFamiliesResponse :: ListTaskDefinitionFamiliesResponse -> TestTree
testListTaskDefinitionFamiliesResponse = res
    "ListTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse.proto"
    ecs
    (Proxy :: Proxy ListTaskDefinitionFamilies)

testStartTaskResponse :: StartTaskResponse -> TestTree
testStartTaskResponse = res
    "StartTaskResponse"
    "fixture/StartTaskResponse.proto"
    ecs
    (Proxy :: Proxy StartTask)

testDeregisterTaskDefinitionResponse :: DeregisterTaskDefinitionResponse -> TestTree
testDeregisterTaskDefinitionResponse = res
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse.proto"
    ecs
    (Proxy :: Proxy DeregisterTaskDefinition)

testDescribeTasksResponse :: DescribeTasksResponse -> TestTree
testDescribeTasksResponse = res
    "DescribeTasksResponse"
    "fixture/DescribeTasksResponse.proto"
    ecs
    (Proxy :: Proxy DescribeTasks)

testListClustersResponse :: ListClustersResponse -> TestTree
testListClustersResponse = res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    ecs
    (Proxy :: Proxy ListClusters)

testDescribeServicesResponse :: DescribeServicesResponse -> TestTree
testDescribeServicesResponse = res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    ecs
    (Proxy :: Proxy DescribeServices)

testDeregisterContainerInstanceResponse :: DeregisterContainerInstanceResponse -> TestTree
testDeregisterContainerInstanceResponse = res
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse.proto"
    ecs
    (Proxy :: Proxy DeregisterContainerInstance)

testRegisterTaskDefinitionResponse :: RegisterTaskDefinitionResponse -> TestTree
testRegisterTaskDefinitionResponse = res
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse.proto"
    ecs
    (Proxy :: Proxy RegisterTaskDefinition)

testCreateServiceResponse :: CreateServiceResponse -> TestTree
testCreateServiceResponse = res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    ecs
    (Proxy :: Proxy CreateService)
