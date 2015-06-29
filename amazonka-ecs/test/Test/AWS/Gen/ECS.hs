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
--         [ createClusterTest $
--             createCluster
--
--         , createServiceTest $
--             createService
--
--         , deleteClusterTest $
--             deleteCluster
--
--         , deleteServiceTest $
--             deleteService
--
--         , deregisterContainerInstanceTest $
--             deregisterContainerInstance
--
--         , deregisterTaskDefinitionTest $
--             deregisterTaskDefinition
--
--         , describeClustersTest $
--             describeClusters
--
--         , describeContainerInstancesTest $
--             describeContainerInstances
--
--         , describeServicesTest $
--             describeServices
--
--         , describeTaskDefinitionTest $
--             describeTaskDefinition
--
--         , describeTasksTest $
--             describeTasks
--
--         , discoverPollEndpointTest $
--             discoverPollEndpoint
--
--         , listClustersTest $
--             listClusters
--
--         , listContainerInstancesTest $
--             listContainerInstances
--
--         , listServicesTest $
--             listServices
--
--         , listTaskDefinitionFamiliesTest $
--             listTaskDefinitionFamilies
--
--         , listTaskDefinitionsTest $
--             listTaskDefinitions
--
--         , listTasksTest $
--             listTasks
--
--         , registerContainerInstanceTest $
--             registerContainerInstance
--
--         , registerTaskDefinitionTest $
--             registerTaskDefinition
--
--         , runTaskTest $
--             runTask
--
--         , startTaskTest $
--             startTask
--
--         , stopTaskTest $
--             stopTask
--
--         , submitContainerStateChangeTest $
--             submitContainerStateChange
--
--         , submitTaskStateChangeTest $
--             submitTaskStateChange
--
--         , updateContainerAgentTest $
--             updateContainerAgent
--
--         , updateServiceTest $
--             updateService
--
--           ]

--     , testGroup "response"
--         [ createClusterResponseTest $
--             createClusterResponse
--
--         , createServiceResponseTest $
--             createServiceResponse
--
--         , deleteClusterResponseTest $
--             deleteClusterResponse
--
--         , deleteServiceResponseTest $
--             deleteServiceResponse
--
--         , deregisterContainerInstanceResponseTest $
--             deregisterContainerInstanceResponse
--
--         , deregisterTaskDefinitionResponseTest $
--             deregisterTaskDefinitionResponse
--
--         , describeClustersResponseTest $
--             describeClustersResponse
--
--         , describeContainerInstancesResponseTest $
--             describeContainerInstancesResponse
--
--         , describeServicesResponseTest $
--             describeServicesResponse
--
--         , describeTaskDefinitionResponseTest $
--             describeTaskDefinitionResponse
--
--         , describeTasksResponseTest $
--             describeTasksResponse
--
--         , discoverPollEndpointResponseTest $
--             discoverPollEndpointResponse
--
--         , listClustersResponseTest $
--             listClustersResponse
--
--         , listContainerInstancesResponseTest $
--             listContainerInstancesResponse
--
--         , listServicesResponseTest $
--             listServicesResponse
--
--         , listTaskDefinitionFamiliesResponseTest $
--             listTaskDefinitionFamiliesResponse
--
--         , listTaskDefinitionsResponseTest $
--             listTaskDefinitionsResponse
--
--         , listTasksResponseTest $
--             listTasksResponse
--
--         , registerContainerInstanceResponseTest $
--             registerContainerInstanceResponse
--
--         , registerTaskDefinitionResponseTest $
--             registerTaskDefinitionResponse
--
--         , runTaskResponseTest $
--             runTaskResponse
--
--         , startTaskResponseTest $
--             startTaskResponse
--
--         , stopTaskResponseTest $
--             stopTaskResponse
--
--         , submitContainerStateChangeResponseTest $
--             submitContainerStateChangeResponse
--
--         , submitTaskStateChangeResponseTest $
--             submitTaskStateChangeResponse
--
--         , updateContainerAgentResponseTest $
--             updateContainerAgentResponse
--
--         , updateServiceResponseTest $
--             updateServiceResponse
--
--           ]
--     ]

-- Requests

createClusterTest :: CreateCluster -> TestTree
createClusterTest = undefined

createServiceTest :: CreateService -> TestTree
createServiceTest = undefined

deleteClusterTest :: DeleteCluster -> TestTree
deleteClusterTest = undefined

deleteServiceTest :: DeleteService -> TestTree
deleteServiceTest = undefined

deregisterContainerInstanceTest :: DeregisterContainerInstance -> TestTree
deregisterContainerInstanceTest = undefined

deregisterTaskDefinitionTest :: DeregisterTaskDefinition -> TestTree
deregisterTaskDefinitionTest = undefined

describeClustersTest :: DescribeClusters -> TestTree
describeClustersTest = undefined

describeContainerInstancesTest :: DescribeContainerInstances -> TestTree
describeContainerInstancesTest = undefined

describeServicesTest :: DescribeServices -> TestTree
describeServicesTest = undefined

describeTaskDefinitionTest :: DescribeTaskDefinition -> TestTree
describeTaskDefinitionTest = undefined

describeTasksTest :: DescribeTasks -> TestTree
describeTasksTest = undefined

discoverPollEndpointTest :: DiscoverPollEndpoint -> TestTree
discoverPollEndpointTest = undefined

listClustersTest :: ListClusters -> TestTree
listClustersTest = undefined

listContainerInstancesTest :: ListContainerInstances -> TestTree
listContainerInstancesTest = undefined

listServicesTest :: ListServices -> TestTree
listServicesTest = undefined

listTaskDefinitionFamiliesTest :: ListTaskDefinitionFamilies -> TestTree
listTaskDefinitionFamiliesTest = undefined

listTaskDefinitionsTest :: ListTaskDefinitions -> TestTree
listTaskDefinitionsTest = undefined

listTasksTest :: ListTasks -> TestTree
listTasksTest = undefined

registerContainerInstanceTest :: RegisterContainerInstance -> TestTree
registerContainerInstanceTest = undefined

registerTaskDefinitionTest :: RegisterTaskDefinition -> TestTree
registerTaskDefinitionTest = undefined

runTaskTest :: RunTask -> TestTree
runTaskTest = undefined

startTaskTest :: StartTask -> TestTree
startTaskTest = undefined

stopTaskTest :: StopTask -> TestTree
stopTaskTest = undefined

submitContainerStateChangeTest :: SubmitContainerStateChange -> TestTree
submitContainerStateChangeTest = undefined

submitTaskStateChangeTest :: SubmitTaskStateChange -> TestTree
submitTaskStateChangeTest = undefined

updateContainerAgentTest :: UpdateContainerAgent -> TestTree
updateContainerAgentTest = undefined

updateServiceTest :: UpdateService -> TestTree
updateServiceTest = undefined

-- Responses

createClusterResponseTest :: CreateClusterResponse -> TestTree
createClusterResponseTest = resp
    "createClusterResponse"
    "fixture/CreateClusterResponse"
    (Proxy :: Proxy CreateCluster)

createServiceResponseTest :: CreateServiceResponse -> TestTree
createServiceResponseTest = resp
    "createServiceResponse"
    "fixture/CreateServiceResponse"
    (Proxy :: Proxy CreateService)

deleteClusterResponseTest :: DeleteClusterResponse -> TestTree
deleteClusterResponseTest = resp
    "deleteClusterResponse"
    "fixture/DeleteClusterResponse"
    (Proxy :: Proxy DeleteCluster)

deleteServiceResponseTest :: DeleteServiceResponse -> TestTree
deleteServiceResponseTest = resp
    "deleteServiceResponse"
    "fixture/DeleteServiceResponse"
    (Proxy :: Proxy DeleteService)

deregisterContainerInstanceResponseTest :: DeregisterContainerInstanceResponse -> TestTree
deregisterContainerInstanceResponseTest = resp
    "deregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse"
    (Proxy :: Proxy DeregisterContainerInstance)

deregisterTaskDefinitionResponseTest :: DeregisterTaskDefinitionResponse -> TestTree
deregisterTaskDefinitionResponseTest = resp
    "deregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse"
    (Proxy :: Proxy DeregisterTaskDefinition)

describeClustersResponseTest :: DescribeClustersResponse -> TestTree
describeClustersResponseTest = resp
    "describeClustersResponse"
    "fixture/DescribeClustersResponse"
    (Proxy :: Proxy DescribeClusters)

describeContainerInstancesResponseTest :: DescribeContainerInstancesResponse -> TestTree
describeContainerInstancesResponseTest = resp
    "describeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse"
    (Proxy :: Proxy DescribeContainerInstances)

describeServicesResponseTest :: DescribeServicesResponse -> TestTree
describeServicesResponseTest = resp
    "describeServicesResponse"
    "fixture/DescribeServicesResponse"
    (Proxy :: Proxy DescribeServices)

describeTaskDefinitionResponseTest :: DescribeTaskDefinitionResponse -> TestTree
describeTaskDefinitionResponseTest = resp
    "describeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse"
    (Proxy :: Proxy DescribeTaskDefinition)

describeTasksResponseTest :: DescribeTasksResponse -> TestTree
describeTasksResponseTest = resp
    "describeTasksResponse"
    "fixture/DescribeTasksResponse"
    (Proxy :: Proxy DescribeTasks)

discoverPollEndpointResponseTest :: DiscoverPollEndpointResponse -> TestTree
discoverPollEndpointResponseTest = resp
    "discoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse"
    (Proxy :: Proxy DiscoverPollEndpoint)

listClustersResponseTest :: ListClustersResponse -> TestTree
listClustersResponseTest = resp
    "listClustersResponse"
    "fixture/ListClustersResponse"
    (Proxy :: Proxy ListClusters)

listContainerInstancesResponseTest :: ListContainerInstancesResponse -> TestTree
listContainerInstancesResponseTest = resp
    "listContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse"
    (Proxy :: Proxy ListContainerInstances)

listServicesResponseTest :: ListServicesResponse -> TestTree
listServicesResponseTest = resp
    "listServicesResponse"
    "fixture/ListServicesResponse"
    (Proxy :: Proxy ListServices)

listTaskDefinitionFamiliesResponseTest :: ListTaskDefinitionFamiliesResponse -> TestTree
listTaskDefinitionFamiliesResponseTest = resp
    "listTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse"
    (Proxy :: Proxy ListTaskDefinitionFamilies)

listTaskDefinitionsResponseTest :: ListTaskDefinitionsResponse -> TestTree
listTaskDefinitionsResponseTest = resp
    "listTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse"
    (Proxy :: Proxy ListTaskDefinitions)

listTasksResponseTest :: ListTasksResponse -> TestTree
listTasksResponseTest = resp
    "listTasksResponse"
    "fixture/ListTasksResponse"
    (Proxy :: Proxy ListTasks)

registerContainerInstanceResponseTest :: RegisterContainerInstanceResponse -> TestTree
registerContainerInstanceResponseTest = resp
    "registerContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse"
    (Proxy :: Proxy RegisterContainerInstance)

registerTaskDefinitionResponseTest :: RegisterTaskDefinitionResponse -> TestTree
registerTaskDefinitionResponseTest = resp
    "registerTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse"
    (Proxy :: Proxy RegisterTaskDefinition)

runTaskResponseTest :: RunTaskResponse -> TestTree
runTaskResponseTest = resp
    "runTaskResponse"
    "fixture/RunTaskResponse"
    (Proxy :: Proxy RunTask)

startTaskResponseTest :: StartTaskResponse -> TestTree
startTaskResponseTest = resp
    "startTaskResponse"
    "fixture/StartTaskResponse"
    (Proxy :: Proxy StartTask)

stopTaskResponseTest :: StopTaskResponse -> TestTree
stopTaskResponseTest = resp
    "stopTaskResponse"
    "fixture/StopTaskResponse"
    (Proxy :: Proxy StopTask)

submitContainerStateChangeResponseTest :: SubmitContainerStateChangeResponse -> TestTree
submitContainerStateChangeResponseTest = resp
    "submitContainerStateChangeResponse"
    "fixture/SubmitContainerStateChangeResponse"
    (Proxy :: Proxy SubmitContainerStateChange)

submitTaskStateChangeResponseTest :: SubmitTaskStateChangeResponse -> TestTree
submitTaskStateChangeResponseTest = resp
    "submitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse"
    (Proxy :: Proxy SubmitTaskStateChange)

updateContainerAgentResponseTest :: UpdateContainerAgentResponse -> TestTree
updateContainerAgentResponseTest = resp
    "updateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse"
    (Proxy :: Proxy UpdateContainerAgent)

updateServiceResponseTest :: UpdateServiceResponse -> TestTree
updateServiceResponseTest = resp
    "updateServiceResponse"
    "fixture/UpdateServiceResponse"
    (Proxy :: Proxy UpdateService)
