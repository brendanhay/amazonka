-- Module      : Test.AWS.Gen.ECS
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ listServicesTest $
--             listServices
--
--         , describeClustersTest $
--             describeClusters
--
--         , deleteServiceTest $
--             deleteService
--
--         , updateServiceTest $
--             updateService
--
--         , discoverPollEndpointTest $
--             discoverPollEndpoint
--
--         , submitContainerStateChangeTest $
--             submitContainerStateChange
--
--         , stopTaskTest $
--             stopTask
--
--         , describeTaskDefinitionTest $
--             describeTaskDefinition
--
--         , submitTaskStateChangeTest $
--             submitTaskStateChange
--
--         , describeContainerInstancesTest $
--             describeContainerInstances
--
--         , deleteClusterTest $
--             deleteCluster
--
--         , createClusterTest $
--             createCluster
--
--         , listTaskDefinitionsTest $
--             listTaskDefinitions
--
--         , listTasksTest $
--             listTasks
--
--         , runTaskTest $
--             runTask
--
--         , listContainerInstancesTest $
--             listContainerInstances
--
--         , registerContainerInstanceTest $
--             registerContainerInstance
--
--         , updateContainerAgentTest $
--             updateContainerAgent
--
--         , listTaskDefinitionFamiliesTest $
--             listTaskDefinitionFamilies
--
--         , startTaskTest $
--             startTask
--
--         , deregisterTaskDefinitionTest $
--             deregisterTaskDefinition
--
--         , describeTasksTest $
--             describeTasks
--
--         , listClustersTest $
--             listClusters
--
--         , describeServicesTest $
--             describeServices
--
--         , deregisterContainerInstanceTest $
--             deregisterContainerInstance
--
--         , registerTaskDefinitionTest $
--             registerTaskDefinition
--
--         , createServiceTest $
--             createService
--
--           ]

--     , testGroup "response"
--         [ listServicesResponseTest $
--             listServicesResponse
--
--         , describeClustersResponseTest $
--             describeClustersResponse
--
--         , deleteServiceResponseTest $
--             deleteServiceResponse
--
--         , updateServiceResponseTest $
--             updateServiceResponse
--
--         , discoverPollEndpointResponseTest $
--             discoverPollEndpointResponse
--
--         , submitContainerStateChangeResponseTest $
--             submitContainerStateChangeResponse
--
--         , stopTaskResponseTest $
--             stopTaskResponse
--
--         , describeTaskDefinitionResponseTest $
--             describeTaskDefinitionResponse
--
--         , submitTaskStateChangeResponseTest $
--             submitTaskStateChangeResponse
--
--         , describeContainerInstancesResponseTest $
--             describeContainerInstancesResponse
--
--         , deleteClusterResponseTest $
--             deleteClusterResponse
--
--         , createClusterResponseTest $
--             createClusterResponse
--
--         , listTaskDefinitionsResponseTest $
--             listTaskDefinitionsResponse
--
--         , listTasksResponseTest $
--             listTasksResponse
--
--         , runTaskResponseTest $
--             runTaskResponse
--
--         , listContainerInstancesResponseTest $
--             listContainerInstancesResponse
--
--         , registerContainerInstanceResponseTest $
--             registerContainerInstanceResponse
--
--         , updateContainerAgentResponseTest $
--             updateContainerAgentResponse
--
--         , listTaskDefinitionFamiliesResponseTest $
--             listTaskDefinitionFamiliesResponse
--
--         , startTaskResponseTest $
--             startTaskResponse
--
--         , deregisterTaskDefinitionResponseTest $
--             deregisterTaskDefinitionResponse
--
--         , describeTasksResponseTest $
--             describeTasksResponse
--
--         , listClustersResponseTest $
--             listClustersResponse
--
--         , describeServicesResponseTest $
--             describeServicesResponse
--
--         , deregisterContainerInstanceResponseTest $
--             deregisterContainerInstanceResponse
--
--         , registerTaskDefinitionResponseTest $
--             registerTaskDefinitionResponse
--
--         , createServiceResponseTest $
--             createServiceResponse
--
--           ]
--     ]

-- Requests

listServicesTest :: ListServices -> TestTree
listServicesTest = undefined

describeClustersTest :: DescribeClusters -> TestTree
describeClustersTest = undefined

deleteServiceTest :: DeleteService -> TestTree
deleteServiceTest = undefined

updateServiceTest :: UpdateService -> TestTree
updateServiceTest = undefined

discoverPollEndpointTest :: DiscoverPollEndpoint -> TestTree
discoverPollEndpointTest = undefined

submitContainerStateChangeTest :: SubmitContainerStateChange -> TestTree
submitContainerStateChangeTest = undefined

stopTaskTest :: StopTask -> TestTree
stopTaskTest = undefined

describeTaskDefinitionTest :: DescribeTaskDefinition -> TestTree
describeTaskDefinitionTest = undefined

submitTaskStateChangeTest :: SubmitTaskStateChange -> TestTree
submitTaskStateChangeTest = undefined

describeContainerInstancesTest :: DescribeContainerInstances -> TestTree
describeContainerInstancesTest = undefined

deleteClusterTest :: DeleteCluster -> TestTree
deleteClusterTest = undefined

createClusterTest :: CreateCluster -> TestTree
createClusterTest = undefined

listTaskDefinitionsTest :: ListTaskDefinitions -> TestTree
listTaskDefinitionsTest = undefined

listTasksTest :: ListTasks -> TestTree
listTasksTest = undefined

runTaskTest :: RunTask -> TestTree
runTaskTest = undefined

listContainerInstancesTest :: ListContainerInstances -> TestTree
listContainerInstancesTest = undefined

registerContainerInstanceTest :: RegisterContainerInstance -> TestTree
registerContainerInstanceTest = undefined

updateContainerAgentTest :: UpdateContainerAgent -> TestTree
updateContainerAgentTest = undefined

listTaskDefinitionFamiliesTest :: ListTaskDefinitionFamilies -> TestTree
listTaskDefinitionFamiliesTest = undefined

startTaskTest :: StartTask -> TestTree
startTaskTest = undefined

deregisterTaskDefinitionTest :: DeregisterTaskDefinition -> TestTree
deregisterTaskDefinitionTest = undefined

describeTasksTest :: DescribeTasks -> TestTree
describeTasksTest = undefined

listClustersTest :: ListClusters -> TestTree
listClustersTest = undefined

describeServicesTest :: DescribeServices -> TestTree
describeServicesTest = undefined

deregisterContainerInstanceTest :: DeregisterContainerInstance -> TestTree
deregisterContainerInstanceTest = undefined

registerTaskDefinitionTest :: RegisterTaskDefinition -> TestTree
registerTaskDefinitionTest = undefined

createServiceTest :: CreateService -> TestTree
createServiceTest = undefined

-- Responses

listServicesResponseTest :: ListServicesResponse -> TestTree
listServicesResponseTest = resp
    "ListServicesResponse"
    "fixture/ListServicesResponse"
    (Proxy :: Proxy ListServices)

describeClustersResponseTest :: DescribeClustersResponse -> TestTree
describeClustersResponseTest = resp
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse"
    (Proxy :: Proxy DescribeClusters)

deleteServiceResponseTest :: DeleteServiceResponse -> TestTree
deleteServiceResponseTest = resp
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse"
    (Proxy :: Proxy DeleteService)

updateServiceResponseTest :: UpdateServiceResponse -> TestTree
updateServiceResponseTest = resp
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse"
    (Proxy :: Proxy UpdateService)

discoverPollEndpointResponseTest :: DiscoverPollEndpointResponse -> TestTree
discoverPollEndpointResponseTest = resp
    "DiscoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse"
    (Proxy :: Proxy DiscoverPollEndpoint)

submitContainerStateChangeResponseTest :: SubmitContainerStateChangeResponse -> TestTree
submitContainerStateChangeResponseTest = resp
    "SubmitContainerStateChangeResponse"
    "fixture/SubmitContainerStateChangeResponse"
    (Proxy :: Proxy SubmitContainerStateChange)

stopTaskResponseTest :: StopTaskResponse -> TestTree
stopTaskResponseTest = resp
    "StopTaskResponse"
    "fixture/StopTaskResponse"
    (Proxy :: Proxy StopTask)

describeTaskDefinitionResponseTest :: DescribeTaskDefinitionResponse -> TestTree
describeTaskDefinitionResponseTest = resp
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse"
    (Proxy :: Proxy DescribeTaskDefinition)

submitTaskStateChangeResponseTest :: SubmitTaskStateChangeResponse -> TestTree
submitTaskStateChangeResponseTest = resp
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse"
    (Proxy :: Proxy SubmitTaskStateChange)

describeContainerInstancesResponseTest :: DescribeContainerInstancesResponse -> TestTree
describeContainerInstancesResponseTest = resp
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse"
    (Proxy :: Proxy DescribeContainerInstances)

deleteClusterResponseTest :: DeleteClusterResponse -> TestTree
deleteClusterResponseTest = resp
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse"
    (Proxy :: Proxy DeleteCluster)

createClusterResponseTest :: CreateClusterResponse -> TestTree
createClusterResponseTest = resp
    "CreateClusterResponse"
    "fixture/CreateClusterResponse"
    (Proxy :: Proxy CreateCluster)

listTaskDefinitionsResponseTest :: ListTaskDefinitionsResponse -> TestTree
listTaskDefinitionsResponseTest = resp
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse"
    (Proxy :: Proxy ListTaskDefinitions)

listTasksResponseTest :: ListTasksResponse -> TestTree
listTasksResponseTest = resp
    "ListTasksResponse"
    "fixture/ListTasksResponse"
    (Proxy :: Proxy ListTasks)

runTaskResponseTest :: RunTaskResponse -> TestTree
runTaskResponseTest = resp
    "RunTaskResponse"
    "fixture/RunTaskResponse"
    (Proxy :: Proxy RunTask)

listContainerInstancesResponseTest :: ListContainerInstancesResponse -> TestTree
listContainerInstancesResponseTest = resp
    "ListContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse"
    (Proxy :: Proxy ListContainerInstances)

registerContainerInstanceResponseTest :: RegisterContainerInstanceResponse -> TestTree
registerContainerInstanceResponseTest = resp
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse"
    (Proxy :: Proxy RegisterContainerInstance)

updateContainerAgentResponseTest :: UpdateContainerAgentResponse -> TestTree
updateContainerAgentResponseTest = resp
    "UpdateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse"
    (Proxy :: Proxy UpdateContainerAgent)

listTaskDefinitionFamiliesResponseTest :: ListTaskDefinitionFamiliesResponse -> TestTree
listTaskDefinitionFamiliesResponseTest = resp
    "ListTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse"
    (Proxy :: Proxy ListTaskDefinitionFamilies)

startTaskResponseTest :: StartTaskResponse -> TestTree
startTaskResponseTest = resp
    "StartTaskResponse"
    "fixture/StartTaskResponse"
    (Proxy :: Proxy StartTask)

deregisterTaskDefinitionResponseTest :: DeregisterTaskDefinitionResponse -> TestTree
deregisterTaskDefinitionResponseTest = resp
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse"
    (Proxy :: Proxy DeregisterTaskDefinition)

describeTasksResponseTest :: DescribeTasksResponse -> TestTree
describeTasksResponseTest = resp
    "DescribeTasksResponse"
    "fixture/DescribeTasksResponse"
    (Proxy :: Proxy DescribeTasks)

listClustersResponseTest :: ListClustersResponse -> TestTree
listClustersResponseTest = resp
    "ListClustersResponse"
    "fixture/ListClustersResponse"
    (Proxy :: Proxy ListClusters)

describeServicesResponseTest :: DescribeServicesResponse -> TestTree
describeServicesResponseTest = resp
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse"
    (Proxy :: Proxy DescribeServices)

deregisterContainerInstanceResponseTest :: DeregisterContainerInstanceResponse -> TestTree
deregisterContainerInstanceResponseTest = resp
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse"
    (Proxy :: Proxy DeregisterContainerInstance)

registerTaskDefinitionResponseTest :: RegisterTaskDefinitionResponse -> TestTree
registerTaskDefinitionResponseTest = resp
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse"
    (Proxy :: Proxy RegisterTaskDefinition)

createServiceResponseTest :: CreateServiceResponse -> TestTree
createServiceResponseTest = resp
    "CreateServiceResponse"
    "fixture/CreateServiceResponse"
    (Proxy :: Proxy CreateService)
