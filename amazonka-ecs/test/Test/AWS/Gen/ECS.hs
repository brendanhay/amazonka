{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ECS where

import Data.Proxy
import Network.AWS.ECS
import Test.AWS.ECS.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListServices $
--             listServices
--
--         , requestDescribeClusters $
--             describeClusters
--
--         , requestDeleteService $
--             deleteService
--
--         , requestUpdateService $
--             updateService
--
--         , requestDiscoverPollEndpoint $
--             discoverPollEndpoint
--
--         , requestSubmitContainerStateChange $
--             submitContainerStateChange
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestStopTask $
--             stopTask
--
--         , requestDescribeTaskDefinition $
--             describeTaskDefinition
--
--         , requestSubmitTaskStateChange $
--             submitTaskStateChange
--
--         , requestDescribeContainerInstances $
--             describeContainerInstances
--
--         , requestUpdateContainerInstancesState $
--             updateContainerInstancesState
--
--         , requestDeleteCluster $
--             deleteCluster
--
--         , requestCreateCluster $
--             createCluster
--
--         , requestPutAccountSetting $
--             putAccountSetting
--
--         , requestDeleteAccountSetting $
--             deleteAccountSetting
--
--         , requestListTaskDefinitions $
--             listTaskDefinitions
--
--         , requestRunTask $
--             runTask
--
--         , requestListTasks $
--             listTasks
--
--         , requestRegisterContainerInstance $
--             registerContainerInstance
--
--         , requestUpdateContainerAgent $
--             updateContainerAgent
--
--         , requestListContainerInstances $
--             listContainerInstances
--
--         , requestListTaskDefinitionFamilies $
--             listTaskDefinitionFamilies
--
--         , requestStartTask $
--             startTask
--
--         , requestPutAccountSettingDefault $
--             putAccountSettingDefault
--
--         , requestListAttributes $
--             listAttributes
--
--         , requestDeregisterTaskDefinition $
--             deregisterTaskDefinition
--
--         , requestTagResource $
--             tagResource
--
--         , requestDescribeTasks $
--             describeTasks
--
--         , requestListClusters $
--             listClusters
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDescribeServices $
--             describeServices
--
--         , requestDeregisterContainerInstance $
--             deregisterContainerInstance
--
--         , requestDeleteAttributes $
--             deleteAttributes
--
--         , requestPutAttributes $
--             putAttributes
--
--         , requestListAccountSettings $
--             listAccountSettings
--
--         , requestRegisterTaskDefinition $
--             registerTaskDefinition
--
--         , requestCreateService $
--             createService
--
--           ]

--     , testGroup "response"
--         [ responseListServices $
--             listServicesResponse
--
--         , responseDescribeClusters $
--             describeClustersResponse
--
--         , responseDeleteService $
--             deleteServiceResponse
--
--         , responseUpdateService $
--             updateServiceResponse
--
--         , responseDiscoverPollEndpoint $
--             discoverPollEndpointResponse
--
--         , responseSubmitContainerStateChange $
--             submitContainerStateChangeResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseStopTask $
--             stopTaskResponse
--
--         , responseDescribeTaskDefinition $
--             describeTaskDefinitionResponse
--
--         , responseSubmitTaskStateChange $
--             submitTaskStateChangeResponse
--
--         , responseDescribeContainerInstances $
--             describeContainerInstancesResponse
--
--         , responseUpdateContainerInstancesState $
--             updateContainerInstancesStateResponse
--
--         , responseDeleteCluster $
--             deleteClusterResponse
--
--         , responseCreateCluster $
--             createClusterResponse
--
--         , responsePutAccountSetting $
--             putAccountSettingResponse
--
--         , responseDeleteAccountSetting $
--             deleteAccountSettingResponse
--
--         , responseListTaskDefinitions $
--             listTaskDefinitionsResponse
--
--         , responseRunTask $
--             runTaskResponse
--
--         , responseListTasks $
--             listTasksResponse
--
--         , responseRegisterContainerInstance $
--             registerContainerInstanceResponse
--
--         , responseUpdateContainerAgent $
--             updateContainerAgentResponse
--
--         , responseListContainerInstances $
--             listContainerInstancesResponse
--
--         , responseListTaskDefinitionFamilies $
--             listTaskDefinitionFamiliesResponse
--
--         , responseStartTask $
--             startTaskResponse
--
--         , responsePutAccountSettingDefault $
--             putAccountSettingDefaultResponse
--
--         , responseListAttributes $
--             listAttributesResponse
--
--         , responseDeregisterTaskDefinition $
--             deregisterTaskDefinitionResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseDescribeTasks $
--             describeTasksResponse
--
--         , responseListClusters $
--             listClustersResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDescribeServices $
--             describeServicesResponse
--
--         , responseDeregisterContainerInstance $
--             deregisterContainerInstanceResponse
--
--         , responseDeleteAttributes $
--             deleteAttributesResponse
--
--         , responsePutAttributes $
--             putAttributesResponse
--
--         , responseListAccountSettings $
--             listAccountSettingsResponse
--
--         , responseRegisterTaskDefinition $
--             registerTaskDefinitionResponse
--
--         , responseCreateService $
--             createServiceResponse
--
--           ]
--     ]

-- Requests

requestListServices :: ListServices -> TestTree
requestListServices = req
    "ListServices"
    "fixture/ListServices.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters = req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDeleteService :: DeleteService -> TestTree
requestDeleteService = req
    "DeleteService"
    "fixture/DeleteService.yaml"

requestUpdateService :: UpdateService -> TestTree
requestUpdateService = req
    "UpdateService"
    "fixture/UpdateService.yaml"

requestDiscoverPollEndpoint :: DiscoverPollEndpoint -> TestTree
requestDiscoverPollEndpoint = req
    "DiscoverPollEndpoint"
    "fixture/DiscoverPollEndpoint.yaml"

requestSubmitContainerStateChange :: SubmitContainerStateChange -> TestTree
requestSubmitContainerStateChange = req
    "SubmitContainerStateChange"
    "fixture/SubmitContainerStateChange.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStopTask :: StopTask -> TestTree
requestStopTask = req
    "StopTask"
    "fixture/StopTask.yaml"

requestDescribeTaskDefinition :: DescribeTaskDefinition -> TestTree
requestDescribeTaskDefinition = req
    "DescribeTaskDefinition"
    "fixture/DescribeTaskDefinition.yaml"

requestSubmitTaskStateChange :: SubmitTaskStateChange -> TestTree
requestSubmitTaskStateChange = req
    "SubmitTaskStateChange"
    "fixture/SubmitTaskStateChange.yaml"

requestDescribeContainerInstances :: DescribeContainerInstances -> TestTree
requestDescribeContainerInstances = req
    "DescribeContainerInstances"
    "fixture/DescribeContainerInstances.yaml"

requestUpdateContainerInstancesState :: UpdateContainerInstancesState -> TestTree
requestUpdateContainerInstancesState = req
    "UpdateContainerInstancesState"
    "fixture/UpdateContainerInstancesState.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestPutAccountSetting :: PutAccountSetting -> TestTree
requestPutAccountSetting = req
    "PutAccountSetting"
    "fixture/PutAccountSetting.yaml"

requestDeleteAccountSetting :: DeleteAccountSetting -> TestTree
requestDeleteAccountSetting = req
    "DeleteAccountSetting"
    "fixture/DeleteAccountSetting.yaml"

requestListTaskDefinitions :: ListTaskDefinitions -> TestTree
requestListTaskDefinitions = req
    "ListTaskDefinitions"
    "fixture/ListTaskDefinitions.yaml"

requestRunTask :: RunTask -> TestTree
requestRunTask = req
    "RunTask"
    "fixture/RunTask.yaml"

requestListTasks :: ListTasks -> TestTree
requestListTasks = req
    "ListTasks"
    "fixture/ListTasks.yaml"

requestRegisterContainerInstance :: RegisterContainerInstance -> TestTree
requestRegisterContainerInstance = req
    "RegisterContainerInstance"
    "fixture/RegisterContainerInstance.yaml"

requestUpdateContainerAgent :: UpdateContainerAgent -> TestTree
requestUpdateContainerAgent = req
    "UpdateContainerAgent"
    "fixture/UpdateContainerAgent.yaml"

requestListContainerInstances :: ListContainerInstances -> TestTree
requestListContainerInstances = req
    "ListContainerInstances"
    "fixture/ListContainerInstances.yaml"

requestListTaskDefinitionFamilies :: ListTaskDefinitionFamilies -> TestTree
requestListTaskDefinitionFamilies = req
    "ListTaskDefinitionFamilies"
    "fixture/ListTaskDefinitionFamilies.yaml"

requestStartTask :: StartTask -> TestTree
requestStartTask = req
    "StartTask"
    "fixture/StartTask.yaml"

requestPutAccountSettingDefault :: PutAccountSettingDefault -> TestTree
requestPutAccountSettingDefault = req
    "PutAccountSettingDefault"
    "fixture/PutAccountSettingDefault.yaml"

requestListAttributes :: ListAttributes -> TestTree
requestListAttributes = req
    "ListAttributes"
    "fixture/ListAttributes.yaml"

requestDeregisterTaskDefinition :: DeregisterTaskDefinition -> TestTree
requestDeregisterTaskDefinition = req
    "DeregisterTaskDefinition"
    "fixture/DeregisterTaskDefinition.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeTasks :: DescribeTasks -> TestTree
requestDescribeTasks = req
    "DescribeTasks"
    "fixture/DescribeTasks.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters = req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeServices :: DescribeServices -> TestTree
requestDescribeServices = req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

requestDeregisterContainerInstance :: DeregisterContainerInstance -> TestTree
requestDeregisterContainerInstance = req
    "DeregisterContainerInstance"
    "fixture/DeregisterContainerInstance.yaml"

requestDeleteAttributes :: DeleteAttributes -> TestTree
requestDeleteAttributes = req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

requestPutAttributes :: PutAttributes -> TestTree
requestPutAttributes = req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

requestListAccountSettings :: ListAccountSettings -> TestTree
requestListAccountSettings = req
    "ListAccountSettings"
    "fixture/ListAccountSettings.yaml"

requestRegisterTaskDefinition :: RegisterTaskDefinition -> TestTree
requestRegisterTaskDefinition = req
    "RegisterTaskDefinition"
    "fixture/RegisterTaskDefinition.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService = req
    "CreateService"
    "fixture/CreateService.yaml"

-- Responses

responseListServices :: ListServicesResponse -> TestTree
responseListServices = res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    ecs
    (Proxy :: Proxy ListServices)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters = res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    ecs
    (Proxy :: Proxy DescribeClusters)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService = res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    ecs
    (Proxy :: Proxy DeleteService)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService = res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    ecs
    (Proxy :: Proxy UpdateService)

responseDiscoverPollEndpoint :: DiscoverPollEndpointResponse -> TestTree
responseDiscoverPollEndpoint = res
    "DiscoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse.proto"
    ecs
    (Proxy :: Proxy DiscoverPollEndpoint)

responseSubmitContainerStateChange :: SubmitContainerStateChangeResponse -> TestTree
responseSubmitContainerStateChange = res
    "SubmitContainerStateChangeResponse"
    "fixture/SubmitContainerStateChangeResponse.proto"
    ecs
    (Proxy :: Proxy SubmitContainerStateChange)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    ecs
    (Proxy :: Proxy ListTagsForResource)

responseStopTask :: StopTaskResponse -> TestTree
responseStopTask = res
    "StopTaskResponse"
    "fixture/StopTaskResponse.proto"
    ecs
    (Proxy :: Proxy StopTask)

responseDescribeTaskDefinition :: DescribeTaskDefinitionResponse -> TestTree
responseDescribeTaskDefinition = res
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse.proto"
    ecs
    (Proxy :: Proxy DescribeTaskDefinition)

responseSubmitTaskStateChange :: SubmitTaskStateChangeResponse -> TestTree
responseSubmitTaskStateChange = res
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse.proto"
    ecs
    (Proxy :: Proxy SubmitTaskStateChange)

responseDescribeContainerInstances :: DescribeContainerInstancesResponse -> TestTree
responseDescribeContainerInstances = res
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse.proto"
    ecs
    (Proxy :: Proxy DescribeContainerInstances)

responseUpdateContainerInstancesState :: UpdateContainerInstancesStateResponse -> TestTree
responseUpdateContainerInstancesState = res
    "UpdateContainerInstancesStateResponse"
    "fixture/UpdateContainerInstancesStateResponse.proto"
    ecs
    (Proxy :: Proxy UpdateContainerInstancesState)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    ecs
    (Proxy :: Proxy DeleteCluster)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    ecs
    (Proxy :: Proxy CreateCluster)

responsePutAccountSetting :: PutAccountSettingResponse -> TestTree
responsePutAccountSetting = res
    "PutAccountSettingResponse"
    "fixture/PutAccountSettingResponse.proto"
    ecs
    (Proxy :: Proxy PutAccountSetting)

responseDeleteAccountSetting :: DeleteAccountSettingResponse -> TestTree
responseDeleteAccountSetting = res
    "DeleteAccountSettingResponse"
    "fixture/DeleteAccountSettingResponse.proto"
    ecs
    (Proxy :: Proxy DeleteAccountSetting)

responseListTaskDefinitions :: ListTaskDefinitionsResponse -> TestTree
responseListTaskDefinitions = res
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse.proto"
    ecs
    (Proxy :: Proxy ListTaskDefinitions)

responseRunTask :: RunTaskResponse -> TestTree
responseRunTask = res
    "RunTaskResponse"
    "fixture/RunTaskResponse.proto"
    ecs
    (Proxy :: Proxy RunTask)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks = res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    ecs
    (Proxy :: Proxy ListTasks)

responseRegisterContainerInstance :: RegisterContainerInstanceResponse -> TestTree
responseRegisterContainerInstance = res
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse.proto"
    ecs
    (Proxy :: Proxy RegisterContainerInstance)

responseUpdateContainerAgent :: UpdateContainerAgentResponse -> TestTree
responseUpdateContainerAgent = res
    "UpdateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse.proto"
    ecs
    (Proxy :: Proxy UpdateContainerAgent)

responseListContainerInstances :: ListContainerInstancesResponse -> TestTree
responseListContainerInstances = res
    "ListContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse.proto"
    ecs
    (Proxy :: Proxy ListContainerInstances)

responseListTaskDefinitionFamilies :: ListTaskDefinitionFamiliesResponse -> TestTree
responseListTaskDefinitionFamilies = res
    "ListTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse.proto"
    ecs
    (Proxy :: Proxy ListTaskDefinitionFamilies)

responseStartTask :: StartTaskResponse -> TestTree
responseStartTask = res
    "StartTaskResponse"
    "fixture/StartTaskResponse.proto"
    ecs
    (Proxy :: Proxy StartTask)

responsePutAccountSettingDefault :: PutAccountSettingDefaultResponse -> TestTree
responsePutAccountSettingDefault = res
    "PutAccountSettingDefaultResponse"
    "fixture/PutAccountSettingDefaultResponse.proto"
    ecs
    (Proxy :: Proxy PutAccountSettingDefault)

responseListAttributes :: ListAttributesResponse -> TestTree
responseListAttributes = res
    "ListAttributesResponse"
    "fixture/ListAttributesResponse.proto"
    ecs
    (Proxy :: Proxy ListAttributes)

responseDeregisterTaskDefinition :: DeregisterTaskDefinitionResponse -> TestTree
responseDeregisterTaskDefinition = res
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse.proto"
    ecs
    (Proxy :: Proxy DeregisterTaskDefinition)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    ecs
    (Proxy :: Proxy TagResource)

responseDescribeTasks :: DescribeTasksResponse -> TestTree
responseDescribeTasks = res
    "DescribeTasksResponse"
    "fixture/DescribeTasksResponse.proto"
    ecs
    (Proxy :: Proxy DescribeTasks)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters = res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    ecs
    (Proxy :: Proxy ListClusters)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    ecs
    (Proxy :: Proxy UntagResource)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices = res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    ecs
    (Proxy :: Proxy DescribeServices)

responseDeregisterContainerInstance :: DeregisterContainerInstanceResponse -> TestTree
responseDeregisterContainerInstance = res
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse.proto"
    ecs
    (Proxy :: Proxy DeregisterContainerInstance)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes = res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    ecs
    (Proxy :: Proxy DeleteAttributes)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes = res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    ecs
    (Proxy :: Proxy PutAttributes)

responseListAccountSettings :: ListAccountSettingsResponse -> TestTree
responseListAccountSettings = res
    "ListAccountSettingsResponse"
    "fixture/ListAccountSettingsResponse.proto"
    ecs
    (Proxy :: Proxy ListAccountSettings)

responseRegisterTaskDefinition :: RegisterTaskDefinitionResponse -> TestTree
responseRegisterTaskDefinition = res
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse.proto"
    ecs
    (Proxy :: Proxy RegisterTaskDefinition)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService = res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    ecs
    (Proxy :: Proxy CreateService)
