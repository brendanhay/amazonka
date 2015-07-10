{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
testListServices = req
    "ListServices"
    "fixture/ListServices"

testDescribeClusters :: DescribeClusters -> TestTree
testDescribeClusters = req
    "DescribeClusters"
    "fixture/DescribeClusters"

testDeleteService :: DeleteService -> TestTree
testDeleteService = req
    "DeleteService"
    "fixture/DeleteService"

testUpdateService :: UpdateService -> TestTree
testUpdateService = req
    "UpdateService"
    "fixture/UpdateService"

testDiscoverPollEndpoint :: DiscoverPollEndpoint -> TestTree
testDiscoverPollEndpoint = req
    "DiscoverPollEndpoint"
    "fixture/DiscoverPollEndpoint"

testSubmitContainerStateChange :: SubmitContainerStateChange -> TestTree
testSubmitContainerStateChange = req
    "SubmitContainerStateChange"
    "fixture/SubmitContainerStateChange"

testStopTask :: StopTask -> TestTree
testStopTask = req
    "StopTask"
    "fixture/StopTask"

testDescribeTaskDefinition :: DescribeTaskDefinition -> TestTree
testDescribeTaskDefinition = req
    "DescribeTaskDefinition"
    "fixture/DescribeTaskDefinition"

testSubmitTaskStateChange :: SubmitTaskStateChange -> TestTree
testSubmitTaskStateChange = req
    "SubmitTaskStateChange"
    "fixture/SubmitTaskStateChange"

testDescribeContainerInstances :: DescribeContainerInstances -> TestTree
testDescribeContainerInstances = req
    "DescribeContainerInstances"
    "fixture/DescribeContainerInstances"

testDeleteCluster :: DeleteCluster -> TestTree
testDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster"

testCreateCluster :: CreateCluster -> TestTree
testCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster"

testListTaskDefinitions :: ListTaskDefinitions -> TestTree
testListTaskDefinitions = req
    "ListTaskDefinitions"
    "fixture/ListTaskDefinitions"

testListTasks :: ListTasks -> TestTree
testListTasks = req
    "ListTasks"
    "fixture/ListTasks"

testRunTask :: RunTask -> TestTree
testRunTask = req
    "RunTask"
    "fixture/RunTask"

testListContainerInstances :: ListContainerInstances -> TestTree
testListContainerInstances = req
    "ListContainerInstances"
    "fixture/ListContainerInstances"

testRegisterContainerInstance :: RegisterContainerInstance -> TestTree
testRegisterContainerInstance = req
    "RegisterContainerInstance"
    "fixture/RegisterContainerInstance"

testUpdateContainerAgent :: UpdateContainerAgent -> TestTree
testUpdateContainerAgent = req
    "UpdateContainerAgent"
    "fixture/UpdateContainerAgent"

testListTaskDefinitionFamilies :: ListTaskDefinitionFamilies -> TestTree
testListTaskDefinitionFamilies = req
    "ListTaskDefinitionFamilies"
    "fixture/ListTaskDefinitionFamilies"

testStartTask :: StartTask -> TestTree
testStartTask = req
    "StartTask"
    "fixture/StartTask"

testDeregisterTaskDefinition :: DeregisterTaskDefinition -> TestTree
testDeregisterTaskDefinition = req
    "DeregisterTaskDefinition"
    "fixture/DeregisterTaskDefinition"

testDescribeTasks :: DescribeTasks -> TestTree
testDescribeTasks = req
    "DescribeTasks"
    "fixture/DescribeTasks"

testListClusters :: ListClusters -> TestTree
testListClusters = req
    "ListClusters"
    "fixture/ListClusters"

testDescribeServices :: DescribeServices -> TestTree
testDescribeServices = req
    "DescribeServices"
    "fixture/DescribeServices"

testDeregisterContainerInstance :: DeregisterContainerInstance -> TestTree
testDeregisterContainerInstance = req
    "DeregisterContainerInstance"
    "fixture/DeregisterContainerInstance"

testRegisterTaskDefinition :: RegisterTaskDefinition -> TestTree
testRegisterTaskDefinition = req
    "RegisterTaskDefinition"
    "fixture/RegisterTaskDefinition"

testCreateService :: CreateService -> TestTree
testCreateService = req
    "CreateService"
    "fixture/CreateService"

-- Responses

testListServicesResponse :: ListServicesResponse -> TestTree
testListServicesResponse = res
    "ListServicesResponse"
    "fixture/ListServicesResponse"
    (Proxy :: Proxy ListServices)

testDescribeClustersResponse :: DescribeClustersResponse -> TestTree
testDescribeClustersResponse = res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse"
    (Proxy :: Proxy DescribeClusters)

testDeleteServiceResponse :: DeleteServiceResponse -> TestTree
testDeleteServiceResponse = res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse"
    (Proxy :: Proxy DeleteService)

testUpdateServiceResponse :: UpdateServiceResponse -> TestTree
testUpdateServiceResponse = res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse"
    (Proxy :: Proxy UpdateService)

testDiscoverPollEndpointResponse :: DiscoverPollEndpointResponse -> TestTree
testDiscoverPollEndpointResponse = res
    "DiscoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse"
    (Proxy :: Proxy DiscoverPollEndpoint)

testSubmitContainerStateChangeResponse :: SubmitContainerStateChangeResponse -> TestTree
testSubmitContainerStateChangeResponse = res
    "SubmitContainerStateChangeResponse"
    "fixture/SubmitContainerStateChangeResponse"
    (Proxy :: Proxy SubmitContainerStateChange)

testStopTaskResponse :: StopTaskResponse -> TestTree
testStopTaskResponse = res
    "StopTaskResponse"
    "fixture/StopTaskResponse"
    (Proxy :: Proxy StopTask)

testDescribeTaskDefinitionResponse :: DescribeTaskDefinitionResponse -> TestTree
testDescribeTaskDefinitionResponse = res
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse"
    (Proxy :: Proxy DescribeTaskDefinition)

testSubmitTaskStateChangeResponse :: SubmitTaskStateChangeResponse -> TestTree
testSubmitTaskStateChangeResponse = res
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse"
    (Proxy :: Proxy SubmitTaskStateChange)

testDescribeContainerInstancesResponse :: DescribeContainerInstancesResponse -> TestTree
testDescribeContainerInstancesResponse = res
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse"
    (Proxy :: Proxy DescribeContainerInstances)

testDeleteClusterResponse :: DeleteClusterResponse -> TestTree
testDeleteClusterResponse = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse"
    (Proxy :: Proxy DeleteCluster)

testCreateClusterResponse :: CreateClusterResponse -> TestTree
testCreateClusterResponse = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse"
    (Proxy :: Proxy CreateCluster)

testListTaskDefinitionsResponse :: ListTaskDefinitionsResponse -> TestTree
testListTaskDefinitionsResponse = res
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse"
    (Proxy :: Proxy ListTaskDefinitions)

testListTasksResponse :: ListTasksResponse -> TestTree
testListTasksResponse = res
    "ListTasksResponse"
    "fixture/ListTasksResponse"
    (Proxy :: Proxy ListTasks)

testRunTaskResponse :: RunTaskResponse -> TestTree
testRunTaskResponse = res
    "RunTaskResponse"
    "fixture/RunTaskResponse"
    (Proxy :: Proxy RunTask)

testListContainerInstancesResponse :: ListContainerInstancesResponse -> TestTree
testListContainerInstancesResponse = res
    "ListContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse"
    (Proxy :: Proxy ListContainerInstances)

testRegisterContainerInstanceResponse :: RegisterContainerInstanceResponse -> TestTree
testRegisterContainerInstanceResponse = res
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse"
    (Proxy :: Proxy RegisterContainerInstance)

testUpdateContainerAgentResponse :: UpdateContainerAgentResponse -> TestTree
testUpdateContainerAgentResponse = res
    "UpdateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse"
    (Proxy :: Proxy UpdateContainerAgent)

testListTaskDefinitionFamiliesResponse :: ListTaskDefinitionFamiliesResponse -> TestTree
testListTaskDefinitionFamiliesResponse = res
    "ListTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse"
    (Proxy :: Proxy ListTaskDefinitionFamilies)

testStartTaskResponse :: StartTaskResponse -> TestTree
testStartTaskResponse = res
    "StartTaskResponse"
    "fixture/StartTaskResponse"
    (Proxy :: Proxy StartTask)

testDeregisterTaskDefinitionResponse :: DeregisterTaskDefinitionResponse -> TestTree
testDeregisterTaskDefinitionResponse = res
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse"
    (Proxy :: Proxy DeregisterTaskDefinition)

testDescribeTasksResponse :: DescribeTasksResponse -> TestTree
testDescribeTasksResponse = res
    "DescribeTasksResponse"
    "fixture/DescribeTasksResponse"
    (Proxy :: Proxy DescribeTasks)

testListClustersResponse :: ListClustersResponse -> TestTree
testListClustersResponse = res
    "ListClustersResponse"
    "fixture/ListClustersResponse"
    (Proxy :: Proxy ListClusters)

testDescribeServicesResponse :: DescribeServicesResponse -> TestTree
testDescribeServicesResponse = res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse"
    (Proxy :: Proxy DescribeServices)

testDeregisterContainerInstanceResponse :: DeregisterContainerInstanceResponse -> TestTree
testDeregisterContainerInstanceResponse = res
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse"
    (Proxy :: Proxy DeregisterContainerInstance)

testRegisterTaskDefinitionResponse :: RegisterTaskDefinitionResponse -> TestTree
testRegisterTaskDefinitionResponse = res
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse"
    (Proxy :: Proxy RegisterTaskDefinition)

testCreateServiceResponse :: CreateServiceResponse -> TestTree
testCreateServiceResponse = res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse"
    (Proxy :: Proxy CreateService)

instance Out AgentUpdateStatus
instance Out Cluster
instance Out Container
instance Out ContainerDefinition
instance Out ContainerInstance
instance Out ContainerOverride
instance Out ContainerService
instance Out CreateCluster
instance Out CreateClusterResponse
instance Out CreateService
instance Out CreateServiceResponse
instance Out DeleteCluster
instance Out DeleteClusterResponse
instance Out DeleteService
instance Out DeleteServiceResponse
instance Out Deployment
instance Out DeregisterContainerInstance
instance Out DeregisterContainerInstanceResponse
instance Out DeregisterTaskDefinition
instance Out DeregisterTaskDefinitionResponse
instance Out DescribeClusters
instance Out DescribeClustersResponse
instance Out DescribeContainerInstances
instance Out DescribeContainerInstancesResponse
instance Out DescribeServices
instance Out DescribeServicesResponse
instance Out DescribeTaskDefinition
instance Out DescribeTaskDefinitionResponse
instance Out DescribeTasks
instance Out DescribeTasksResponse
instance Out DesiredStatus
instance Out DiscoverPollEndpoint
instance Out DiscoverPollEndpointResponse
instance Out Failure
instance Out HostVolumeProperties
instance Out KeyValuePair
instance Out ListClusters
instance Out ListClustersResponse
instance Out ListContainerInstances
instance Out ListContainerInstancesResponse
instance Out ListServices
instance Out ListServicesResponse
instance Out ListTaskDefinitionFamilies
instance Out ListTaskDefinitionFamiliesResponse
instance Out ListTaskDefinitions
instance Out ListTaskDefinitionsResponse
instance Out ListTasks
instance Out ListTasksResponse
instance Out LoadBalancer
instance Out MountPoint
instance Out NetworkBinding
instance Out PortMapping
instance Out RegisterContainerInstance
instance Out RegisterContainerInstanceResponse
instance Out RegisterTaskDefinition
instance Out RegisterTaskDefinitionResponse
instance Out Resource
instance Out RunTask
instance Out RunTaskResponse
instance Out ServiceEvent
instance Out SortOrder
instance Out StartTask
instance Out StartTaskResponse
instance Out StopTask
instance Out StopTaskResponse
instance Out SubmitContainerStateChange
instance Out SubmitContainerStateChangeResponse
instance Out SubmitTaskStateChange
instance Out SubmitTaskStateChangeResponse
instance Out Task
instance Out TaskDefinition
instance Out TaskDefinitionStatus
instance Out TaskOverride
instance Out TransportProtocol
instance Out UpdateContainerAgent
instance Out UpdateContainerAgentResponse
instance Out UpdateService
instance Out UpdateServiceResponse
instance Out VersionInfo
instance Out Volume
instance Out VolumeFrom
