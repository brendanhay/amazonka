-- Module      : Test.AWS.Gen.ECS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.ECS where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.ECS

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
--         , testListTasks $
--             listTasks
--
--         , testRunTask $
--             runTask
--
--         , testListContainerInstances $
--             listContainerInstances
--
--         , testRegisterContainerInstance $
--             registerContainerInstance
--
--         , testUpdateContainerAgent $
--             updateContainerAgent
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
--         , testListTasksResponse $
--             listTasksResponse
--
--         , testRunTaskResponse $
--             runTaskResponse
--
--         , testListContainerInstancesResponse $
--             listContainerInstancesResponse
--
--         , testRegisterContainerInstanceResponse $
--             registerContainerInstanceResponse
--
--         , testUpdateContainerAgentResponse $
--             updateContainerAgentResponse
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
testListServices = undefined

testDescribeClusters :: DescribeClusters -> TestTree
testDescribeClusters = undefined

testDeleteService :: DeleteService -> TestTree
testDeleteService = undefined

testUpdateService :: UpdateService -> TestTree
testUpdateService = undefined

testDiscoverPollEndpoint :: DiscoverPollEndpoint -> TestTree
testDiscoverPollEndpoint = undefined

testSubmitContainerStateChange :: SubmitContainerStateChange -> TestTree
testSubmitContainerStateChange = undefined

testStopTask :: StopTask -> TestTree
testStopTask = undefined

testDescribeTaskDefinition :: DescribeTaskDefinition -> TestTree
testDescribeTaskDefinition = undefined

testSubmitTaskStateChange :: SubmitTaskStateChange -> TestTree
testSubmitTaskStateChange = undefined

testDescribeContainerInstances :: DescribeContainerInstances -> TestTree
testDescribeContainerInstances = undefined

testDeleteCluster :: DeleteCluster -> TestTree
testDeleteCluster = undefined

testCreateCluster :: CreateCluster -> TestTree
testCreateCluster = undefined

testListTaskDefinitions :: ListTaskDefinitions -> TestTree
testListTaskDefinitions = undefined

testListTasks :: ListTasks -> TestTree
testListTasks = undefined

testRunTask :: RunTask -> TestTree
testRunTask = undefined

testListContainerInstances :: ListContainerInstances -> TestTree
testListContainerInstances = undefined

testRegisterContainerInstance :: RegisterContainerInstance -> TestTree
testRegisterContainerInstance = undefined

testUpdateContainerAgent :: UpdateContainerAgent -> TestTree
testUpdateContainerAgent = undefined

testListTaskDefinitionFamilies :: ListTaskDefinitionFamilies -> TestTree
testListTaskDefinitionFamilies = undefined

testStartTask :: StartTask -> TestTree
testStartTask = undefined

testDeregisterTaskDefinition :: DeregisterTaskDefinition -> TestTree
testDeregisterTaskDefinition = undefined

testDescribeTasks :: DescribeTasks -> TestTree
testDescribeTasks = undefined

testListClusters :: ListClusters -> TestTree
testListClusters = undefined

testDescribeServices :: DescribeServices -> TestTree
testDescribeServices = undefined

testDeregisterContainerInstance :: DeregisterContainerInstance -> TestTree
testDeregisterContainerInstance = undefined

testRegisterTaskDefinition :: RegisterTaskDefinition -> TestTree
testRegisterTaskDefinition = undefined

testCreateService :: CreateService -> TestTree
testCreateService = undefined

-- Responses

testListServicesResponse :: ListServicesResponse -> TestTree
testListServicesResponse = resp
    "ListServicesResponse"
    "fixture/ListServicesResponse"
    (Proxy :: Proxy ListServices)

testDescribeClustersResponse :: DescribeClustersResponse -> TestTree
testDescribeClustersResponse = resp
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse"
    (Proxy :: Proxy DescribeClusters)

testDeleteServiceResponse :: DeleteServiceResponse -> TestTree
testDeleteServiceResponse = resp
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse"
    (Proxy :: Proxy DeleteService)

testUpdateServiceResponse :: UpdateServiceResponse -> TestTree
testUpdateServiceResponse = resp
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse"
    (Proxy :: Proxy UpdateService)

testDiscoverPollEndpointResponse :: DiscoverPollEndpointResponse -> TestTree
testDiscoverPollEndpointResponse = resp
    "DiscoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse"
    (Proxy :: Proxy DiscoverPollEndpoint)

testSubmitContainerStateChangeResponse :: SubmitContainerStateChangeResponse -> TestTree
testSubmitContainerStateChangeResponse = resp
    "SubmitContainerStateChangeResponse"
    "fixture/SubmitContainerStateChangeResponse"
    (Proxy :: Proxy SubmitContainerStateChange)

testStopTaskResponse :: StopTaskResponse -> TestTree
testStopTaskResponse = resp
    "StopTaskResponse"
    "fixture/StopTaskResponse"
    (Proxy :: Proxy StopTask)

testDescribeTaskDefinitionResponse :: DescribeTaskDefinitionResponse -> TestTree
testDescribeTaskDefinitionResponse = resp
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse"
    (Proxy :: Proxy DescribeTaskDefinition)

testSubmitTaskStateChangeResponse :: SubmitTaskStateChangeResponse -> TestTree
testSubmitTaskStateChangeResponse = resp
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse"
    (Proxy :: Proxy SubmitTaskStateChange)

testDescribeContainerInstancesResponse :: DescribeContainerInstancesResponse -> TestTree
testDescribeContainerInstancesResponse = resp
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse"
    (Proxy :: Proxy DescribeContainerInstances)

testDeleteClusterResponse :: DeleteClusterResponse -> TestTree
testDeleteClusterResponse = resp
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse"
    (Proxy :: Proxy DeleteCluster)

testCreateClusterResponse :: CreateClusterResponse -> TestTree
testCreateClusterResponse = resp
    "CreateClusterResponse"
    "fixture/CreateClusterResponse"
    (Proxy :: Proxy CreateCluster)

testListTaskDefinitionsResponse :: ListTaskDefinitionsResponse -> TestTree
testListTaskDefinitionsResponse = resp
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse"
    (Proxy :: Proxy ListTaskDefinitions)

testListTasksResponse :: ListTasksResponse -> TestTree
testListTasksResponse = resp
    "ListTasksResponse"
    "fixture/ListTasksResponse"
    (Proxy :: Proxy ListTasks)

testRunTaskResponse :: RunTaskResponse -> TestTree
testRunTaskResponse = resp
    "RunTaskResponse"
    "fixture/RunTaskResponse"
    (Proxy :: Proxy RunTask)

testListContainerInstancesResponse :: ListContainerInstancesResponse -> TestTree
testListContainerInstancesResponse = resp
    "ListContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse"
    (Proxy :: Proxy ListContainerInstances)

testRegisterContainerInstanceResponse :: RegisterContainerInstanceResponse -> TestTree
testRegisterContainerInstanceResponse = resp
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse"
    (Proxy :: Proxy RegisterContainerInstance)

testUpdateContainerAgentResponse :: UpdateContainerAgentResponse -> TestTree
testUpdateContainerAgentResponse = resp
    "UpdateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse"
    (Proxy :: Proxy UpdateContainerAgent)

testListTaskDefinitionFamiliesResponse :: ListTaskDefinitionFamiliesResponse -> TestTree
testListTaskDefinitionFamiliesResponse = resp
    "ListTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse"
    (Proxy :: Proxy ListTaskDefinitionFamilies)

testStartTaskResponse :: StartTaskResponse -> TestTree
testStartTaskResponse = resp
    "StartTaskResponse"
    "fixture/StartTaskResponse"
    (Proxy :: Proxy StartTask)

testDeregisterTaskDefinitionResponse :: DeregisterTaskDefinitionResponse -> TestTree
testDeregisterTaskDefinitionResponse = resp
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse"
    (Proxy :: Proxy DeregisterTaskDefinition)

testDescribeTasksResponse :: DescribeTasksResponse -> TestTree
testDescribeTasksResponse = resp
    "DescribeTasksResponse"
    "fixture/DescribeTasksResponse"
    (Proxy :: Proxy DescribeTasks)

testListClustersResponse :: ListClustersResponse -> TestTree
testListClustersResponse = resp
    "ListClustersResponse"
    "fixture/ListClustersResponse"
    (Proxy :: Proxy ListClusters)

testDescribeServicesResponse :: DescribeServicesResponse -> TestTree
testDescribeServicesResponse = resp
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse"
    (Proxy :: Proxy DescribeServices)

testDeregisterContainerInstanceResponse :: DeregisterContainerInstanceResponse -> TestTree
testDeregisterContainerInstanceResponse = resp
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse"
    (Proxy :: Proxy DeregisterContainerInstance)

testRegisterTaskDefinitionResponse :: RegisterTaskDefinitionResponse -> TestTree
testRegisterTaskDefinitionResponse = resp
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse"
    (Proxy :: Proxy RegisterTaskDefinition)

testCreateServiceResponse :: CreateServiceResponse -> TestTree
testCreateServiceResponse = resp
    "CreateServiceResponse"
    "fixture/CreateServiceResponse"
    (Proxy :: Proxy CreateService)
