{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkListServices
--
--         , requestDescribeClusters $
--             mkDescribeClusters
--
--         , requestDeleteService $
--             mkDeleteService
--
--         , requestUpdateService $
--             mkUpdateService
--
--         , requestDiscoverPollEndpoint $
--             mkDiscoverPollEndpoint
--
--         , requestSubmitAttachmentStateChanges $
--             mkSubmitAttachmentStateChanges
--
--         , requestSubmitContainerStateChange $
--             mkSubmitContainerStateChange
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestStopTask $
--             mkStopTask
--
--         , requestDescribeTaskDefinition $
--             mkDescribeTaskDefinition
--
--         , requestSubmitTaskStateChange $
--             mkSubmitTaskStateChange
--
--         , requestDescribeContainerInstances $
--             mkDescribeContainerInstances
--
--         , requestDescribeCapacityProviders $
--             mkDescribeCapacityProviders
--
--         , requestUpdateContainerInstancesState $
--             mkUpdateContainerInstancesState
--
--         , requestDeleteCluster $
--             mkDeleteCluster
--
--         , requestCreateCluster $
--             mkCreateCluster
--
--         , requestPutAccountSetting $
--             mkPutAccountSetting
--
--         , requestDeleteAccountSetting $
--             mkDeleteAccountSetting
--
--         , requestListTaskDefinitions $
--             mkListTaskDefinitions
--
--         , requestRunTask $
--             mkRunTask
--
--         , requestDeleteCapacityProvider $
--             mkDeleteCapacityProvider
--
--         , requestListTasks $
--             mkListTasks
--
--         , requestUpdateCapacityProvider $
--             mkUpdateCapacityProvider
--
--         , requestRegisterContainerInstance $
--             mkRegisterContainerInstance
--
--         , requestUpdateContainerAgent $
--             mkUpdateContainerAgent
--
--         , requestListContainerInstances $
--             mkListContainerInstances
--
--         , requestUpdateServicePrimaryTaskSet $
--             mkUpdateServicePrimaryTaskSet
--
--         , requestListTaskDefinitionFamilies $
--             mkListTaskDefinitionFamilies
--
--         , requestStartTask $
--             mkStartTask
--
--         , requestPutClusterCapacityProviders $
--             mkPutClusterCapacityProviders
--
--         , requestPutAccountSettingDefault $
--             mkPutAccountSettingDefault
--
--         , requestListAttributes $
--             mkListAttributes
--
--         , requestDeregisterTaskDefinition $
--             mkDeregisterTaskDefinition
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestCreateTaskSet $
--             mkCreateTaskSet
--
--         , requestDescribeTasks $
--             mkDescribeTasks
--
--         , requestListClusters $
--             mkListClusters
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDescribeServices $
--             mkDescribeServices
--
--         , requestDeregisterContainerInstance $
--             mkDeregisterContainerInstance
--
--         , requestUpdateClusterSettings $
--             mkUpdateClusterSettings
--
--         , requestDeleteAttributes $
--             mkDeleteAttributes
--
--         , requestPutAttributes $
--             mkPutAttributes
--
--         , requestListAccountSettings $
--             mkListAccountSettings
--
--         , requestDeleteTaskSet $
--             mkDeleteTaskSet
--
--         , requestUpdateTaskSet $
--             mkUpdateTaskSet
--
--         , requestCreateCapacityProvider $
--             mkCreateCapacityProvider
--
--         , requestDescribeTaskSets $
--             mkDescribeTaskSets
--
--         , requestRegisterTaskDefinition $
--             mkRegisterTaskDefinition
--
--         , requestCreateService $
--             mkCreateService
--
--           ]

--     , testGroup "response"
--         [ responseListServices $
--             mkListServicesResponse
--
--         , responseDescribeClusters $
--             mkDescribeClustersResponse
--
--         , responseDeleteService $
--             mkDeleteServiceResponse
--
--         , responseUpdateService $
--             mkUpdateServiceResponse
--
--         , responseDiscoverPollEndpoint $
--             mkDiscoverPollEndpointResponse
--
--         , responseSubmitAttachmentStateChanges $
--             mkSubmitAttachmentStateChangesResponse
--
--         , responseSubmitContainerStateChange $
--             mkSubmitContainerStateChangeResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseStopTask $
--             mkStopTaskResponse
--
--         , responseDescribeTaskDefinition $
--             mkDescribeTaskDefinitionResponse
--
--         , responseSubmitTaskStateChange $
--             mkSubmitTaskStateChangeResponse
--
--         , responseDescribeContainerInstances $
--             mkDescribeContainerInstancesResponse
--
--         , responseDescribeCapacityProviders $
--             mkDescribeCapacityProvidersResponse
--
--         , responseUpdateContainerInstancesState $
--             mkUpdateContainerInstancesStateResponse
--
--         , responseDeleteCluster $
--             mkDeleteClusterResponse
--
--         , responseCreateCluster $
--             mkCreateClusterResponse
--
--         , responsePutAccountSetting $
--             mkPutAccountSettingResponse
--
--         , responseDeleteAccountSetting $
--             mkDeleteAccountSettingResponse
--
--         , responseListTaskDefinitions $
--             mkListTaskDefinitionsResponse
--
--         , responseRunTask $
--             mkRunTaskResponse
--
--         , responseDeleteCapacityProvider $
--             mkDeleteCapacityProviderResponse
--
--         , responseListTasks $
--             mkListTasksResponse
--
--         , responseUpdateCapacityProvider $
--             mkUpdateCapacityProviderResponse
--
--         , responseRegisterContainerInstance $
--             mkRegisterContainerInstanceResponse
--
--         , responseUpdateContainerAgent $
--             mkUpdateContainerAgentResponse
--
--         , responseListContainerInstances $
--             mkListContainerInstancesResponse
--
--         , responseUpdateServicePrimaryTaskSet $
--             mkUpdateServicePrimaryTaskSetResponse
--
--         , responseListTaskDefinitionFamilies $
--             mkListTaskDefinitionFamiliesResponse
--
--         , responseStartTask $
--             mkStartTaskResponse
--
--         , responsePutClusterCapacityProviders $
--             mkPutClusterCapacityProvidersResponse
--
--         , responsePutAccountSettingDefault $
--             mkPutAccountSettingDefaultResponse
--
--         , responseListAttributes $
--             mkListAttributesResponse
--
--         , responseDeregisterTaskDefinition $
--             mkDeregisterTaskDefinitionResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseCreateTaskSet $
--             mkCreateTaskSetResponse
--
--         , responseDescribeTasks $
--             mkDescribeTasksResponse
--
--         , responseListClusters $
--             mkListClustersResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDescribeServices $
--             mkDescribeServicesResponse
--
--         , responseDeregisterContainerInstance $
--             mkDeregisterContainerInstanceResponse
--
--         , responseUpdateClusterSettings $
--             mkUpdateClusterSettingsResponse
--
--         , responseDeleteAttributes $
--             mkDeleteAttributesResponse
--
--         , responsePutAttributes $
--             mkPutAttributesResponse
--
--         , responseListAccountSettings $
--             mkListAccountSettingsResponse
--
--         , responseDeleteTaskSet $
--             mkDeleteTaskSetResponse
--
--         , responseUpdateTaskSet $
--             mkUpdateTaskSetResponse
--
--         , responseCreateCapacityProvider $
--             mkCreateCapacityProviderResponse
--
--         , responseDescribeTaskSets $
--             mkDescribeTaskSetsResponse
--
--         , responseRegisterTaskDefinition $
--             mkRegisterTaskDefinitionResponse
--
--         , responseCreateService $
--             mkCreateServiceResponse
--
--           ]
--     ]

-- Requests

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDeleteService :: DeleteService -> TestTree
requestDeleteService =
  req
    "DeleteService"
    "fixture/DeleteService.yaml"

requestUpdateService :: UpdateService -> TestTree
requestUpdateService =
  req
    "UpdateService"
    "fixture/UpdateService.yaml"

requestDiscoverPollEndpoint :: DiscoverPollEndpoint -> TestTree
requestDiscoverPollEndpoint =
  req
    "DiscoverPollEndpoint"
    "fixture/DiscoverPollEndpoint.yaml"

requestSubmitAttachmentStateChanges :: SubmitAttachmentStateChanges -> TestTree
requestSubmitAttachmentStateChanges =
  req
    "SubmitAttachmentStateChanges"
    "fixture/SubmitAttachmentStateChanges.yaml"

requestSubmitContainerStateChange :: SubmitContainerStateChange -> TestTree
requestSubmitContainerStateChange =
  req
    "SubmitContainerStateChange"
    "fixture/SubmitContainerStateChange.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStopTask :: StopTask -> TestTree
requestStopTask =
  req
    "StopTask"
    "fixture/StopTask.yaml"

requestDescribeTaskDefinition :: DescribeTaskDefinition -> TestTree
requestDescribeTaskDefinition =
  req
    "DescribeTaskDefinition"
    "fixture/DescribeTaskDefinition.yaml"

requestSubmitTaskStateChange :: SubmitTaskStateChange -> TestTree
requestSubmitTaskStateChange =
  req
    "SubmitTaskStateChange"
    "fixture/SubmitTaskStateChange.yaml"

requestDescribeContainerInstances :: DescribeContainerInstances -> TestTree
requestDescribeContainerInstances =
  req
    "DescribeContainerInstances"
    "fixture/DescribeContainerInstances.yaml"

requestDescribeCapacityProviders :: DescribeCapacityProviders -> TestTree
requestDescribeCapacityProviders =
  req
    "DescribeCapacityProviders"
    "fixture/DescribeCapacityProviders.yaml"

requestUpdateContainerInstancesState :: UpdateContainerInstancesState -> TestTree
requestUpdateContainerInstancesState =
  req
    "UpdateContainerInstancesState"
    "fixture/UpdateContainerInstancesState.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestPutAccountSetting :: PutAccountSetting -> TestTree
requestPutAccountSetting =
  req
    "PutAccountSetting"
    "fixture/PutAccountSetting.yaml"

requestDeleteAccountSetting :: DeleteAccountSetting -> TestTree
requestDeleteAccountSetting =
  req
    "DeleteAccountSetting"
    "fixture/DeleteAccountSetting.yaml"

requestListTaskDefinitions :: ListTaskDefinitions -> TestTree
requestListTaskDefinitions =
  req
    "ListTaskDefinitions"
    "fixture/ListTaskDefinitions.yaml"

requestRunTask :: RunTask -> TestTree
requestRunTask =
  req
    "RunTask"
    "fixture/RunTask.yaml"

requestDeleteCapacityProvider :: DeleteCapacityProvider -> TestTree
requestDeleteCapacityProvider =
  req
    "DeleteCapacityProvider"
    "fixture/DeleteCapacityProvider.yaml"

requestListTasks :: ListTasks -> TestTree
requestListTasks =
  req
    "ListTasks"
    "fixture/ListTasks.yaml"

requestUpdateCapacityProvider :: UpdateCapacityProvider -> TestTree
requestUpdateCapacityProvider =
  req
    "UpdateCapacityProvider"
    "fixture/UpdateCapacityProvider.yaml"

requestRegisterContainerInstance :: RegisterContainerInstance -> TestTree
requestRegisterContainerInstance =
  req
    "RegisterContainerInstance"
    "fixture/RegisterContainerInstance.yaml"

requestUpdateContainerAgent :: UpdateContainerAgent -> TestTree
requestUpdateContainerAgent =
  req
    "UpdateContainerAgent"
    "fixture/UpdateContainerAgent.yaml"

requestListContainerInstances :: ListContainerInstances -> TestTree
requestListContainerInstances =
  req
    "ListContainerInstances"
    "fixture/ListContainerInstances.yaml"

requestUpdateServicePrimaryTaskSet :: UpdateServicePrimaryTaskSet -> TestTree
requestUpdateServicePrimaryTaskSet =
  req
    "UpdateServicePrimaryTaskSet"
    "fixture/UpdateServicePrimaryTaskSet.yaml"

requestListTaskDefinitionFamilies :: ListTaskDefinitionFamilies -> TestTree
requestListTaskDefinitionFamilies =
  req
    "ListTaskDefinitionFamilies"
    "fixture/ListTaskDefinitionFamilies.yaml"

requestStartTask :: StartTask -> TestTree
requestStartTask =
  req
    "StartTask"
    "fixture/StartTask.yaml"

requestPutClusterCapacityProviders :: PutClusterCapacityProviders -> TestTree
requestPutClusterCapacityProviders =
  req
    "PutClusterCapacityProviders"
    "fixture/PutClusterCapacityProviders.yaml"

requestPutAccountSettingDefault :: PutAccountSettingDefault -> TestTree
requestPutAccountSettingDefault =
  req
    "PutAccountSettingDefault"
    "fixture/PutAccountSettingDefault.yaml"

requestListAttributes :: ListAttributes -> TestTree
requestListAttributes =
  req
    "ListAttributes"
    "fixture/ListAttributes.yaml"

requestDeregisterTaskDefinition :: DeregisterTaskDefinition -> TestTree
requestDeregisterTaskDefinition =
  req
    "DeregisterTaskDefinition"
    "fixture/DeregisterTaskDefinition.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateTaskSet :: CreateTaskSet -> TestTree
requestCreateTaskSet =
  req
    "CreateTaskSet"
    "fixture/CreateTaskSet.yaml"

requestDescribeTasks :: DescribeTasks -> TestTree
requestDescribeTasks =
  req
    "DescribeTasks"
    "fixture/DescribeTasks.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeServices :: DescribeServices -> TestTree
requestDescribeServices =
  req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

requestDeregisterContainerInstance :: DeregisterContainerInstance -> TestTree
requestDeregisterContainerInstance =
  req
    "DeregisterContainerInstance"
    "fixture/DeregisterContainerInstance.yaml"

requestUpdateClusterSettings :: UpdateClusterSettings -> TestTree
requestUpdateClusterSettings =
  req
    "UpdateClusterSettings"
    "fixture/UpdateClusterSettings.yaml"

requestDeleteAttributes :: DeleteAttributes -> TestTree
requestDeleteAttributes =
  req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

requestPutAttributes :: PutAttributes -> TestTree
requestPutAttributes =
  req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

requestListAccountSettings :: ListAccountSettings -> TestTree
requestListAccountSettings =
  req
    "ListAccountSettings"
    "fixture/ListAccountSettings.yaml"

requestDeleteTaskSet :: DeleteTaskSet -> TestTree
requestDeleteTaskSet =
  req
    "DeleteTaskSet"
    "fixture/DeleteTaskSet.yaml"

requestUpdateTaskSet :: UpdateTaskSet -> TestTree
requestUpdateTaskSet =
  req
    "UpdateTaskSet"
    "fixture/UpdateTaskSet.yaml"

requestCreateCapacityProvider :: CreateCapacityProvider -> TestTree
requestCreateCapacityProvider =
  req
    "CreateCapacityProvider"
    "fixture/CreateCapacityProvider.yaml"

requestDescribeTaskSets :: DescribeTaskSets -> TestTree
requestDescribeTaskSets =
  req
    "DescribeTaskSets"
    "fixture/DescribeTaskSets.yaml"

requestRegisterTaskDefinition :: RegisterTaskDefinition -> TestTree
requestRegisterTaskDefinition =
  req
    "RegisterTaskDefinition"
    "fixture/RegisterTaskDefinition.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

-- Responses

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    ecsService
    (Proxy :: Proxy ListServices)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    ecsService
    (Proxy :: Proxy DescribeClusters)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    ecsService
    (Proxy :: Proxy DeleteService)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    ecsService
    (Proxy :: Proxy UpdateService)

responseDiscoverPollEndpoint :: DiscoverPollEndpointResponse -> TestTree
responseDiscoverPollEndpoint =
  res
    "DiscoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse.proto"
    ecsService
    (Proxy :: Proxy DiscoverPollEndpoint)

responseSubmitAttachmentStateChanges :: SubmitAttachmentStateChangesResponse -> TestTree
responseSubmitAttachmentStateChanges =
  res
    "SubmitAttachmentStateChangesResponse"
    "fixture/SubmitAttachmentStateChangesResponse.proto"
    ecsService
    (Proxy :: Proxy SubmitAttachmentStateChanges)

responseSubmitContainerStateChange :: SubmitContainerStateChangeResponse -> TestTree
responseSubmitContainerStateChange =
  res
    "SubmitContainerStateChangeResponse"
    "fixture/SubmitContainerStateChangeResponse.proto"
    ecsService
    (Proxy :: Proxy SubmitContainerStateChange)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    ecsService
    (Proxy :: Proxy ListTagsForResource)

responseStopTask :: StopTaskResponse -> TestTree
responseStopTask =
  res
    "StopTaskResponse"
    "fixture/StopTaskResponse.proto"
    ecsService
    (Proxy :: Proxy StopTask)

responseDescribeTaskDefinition :: DescribeTaskDefinitionResponse -> TestTree
responseDescribeTaskDefinition =
  res
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse.proto"
    ecsService
    (Proxy :: Proxy DescribeTaskDefinition)

responseSubmitTaskStateChange :: SubmitTaskStateChangeResponse -> TestTree
responseSubmitTaskStateChange =
  res
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse.proto"
    ecsService
    (Proxy :: Proxy SubmitTaskStateChange)

responseDescribeContainerInstances :: DescribeContainerInstancesResponse -> TestTree
responseDescribeContainerInstances =
  res
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse.proto"
    ecsService
    (Proxy :: Proxy DescribeContainerInstances)

responseDescribeCapacityProviders :: DescribeCapacityProvidersResponse -> TestTree
responseDescribeCapacityProviders =
  res
    "DescribeCapacityProvidersResponse"
    "fixture/DescribeCapacityProvidersResponse.proto"
    ecsService
    (Proxy :: Proxy DescribeCapacityProviders)

responseUpdateContainerInstancesState :: UpdateContainerInstancesStateResponse -> TestTree
responseUpdateContainerInstancesState =
  res
    "UpdateContainerInstancesStateResponse"
    "fixture/UpdateContainerInstancesStateResponse.proto"
    ecsService
    (Proxy :: Proxy UpdateContainerInstancesState)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    ecsService
    (Proxy :: Proxy DeleteCluster)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    ecsService
    (Proxy :: Proxy CreateCluster)

responsePutAccountSetting :: PutAccountSettingResponse -> TestTree
responsePutAccountSetting =
  res
    "PutAccountSettingResponse"
    "fixture/PutAccountSettingResponse.proto"
    ecsService
    (Proxy :: Proxy PutAccountSetting)

responseDeleteAccountSetting :: DeleteAccountSettingResponse -> TestTree
responseDeleteAccountSetting =
  res
    "DeleteAccountSettingResponse"
    "fixture/DeleteAccountSettingResponse.proto"
    ecsService
    (Proxy :: Proxy DeleteAccountSetting)

responseListTaskDefinitions :: ListTaskDefinitionsResponse -> TestTree
responseListTaskDefinitions =
  res
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse.proto"
    ecsService
    (Proxy :: Proxy ListTaskDefinitions)

responseRunTask :: RunTaskResponse -> TestTree
responseRunTask =
  res
    "RunTaskResponse"
    "fixture/RunTaskResponse.proto"
    ecsService
    (Proxy :: Proxy RunTask)

responseDeleteCapacityProvider :: DeleteCapacityProviderResponse -> TestTree
responseDeleteCapacityProvider =
  res
    "DeleteCapacityProviderResponse"
    "fixture/DeleteCapacityProviderResponse.proto"
    ecsService
    (Proxy :: Proxy DeleteCapacityProvider)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks =
  res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    ecsService
    (Proxy :: Proxy ListTasks)

responseUpdateCapacityProvider :: UpdateCapacityProviderResponse -> TestTree
responseUpdateCapacityProvider =
  res
    "UpdateCapacityProviderResponse"
    "fixture/UpdateCapacityProviderResponse.proto"
    ecsService
    (Proxy :: Proxy UpdateCapacityProvider)

responseRegisterContainerInstance :: RegisterContainerInstanceResponse -> TestTree
responseRegisterContainerInstance =
  res
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse.proto"
    ecsService
    (Proxy :: Proxy RegisterContainerInstance)

responseUpdateContainerAgent :: UpdateContainerAgentResponse -> TestTree
responseUpdateContainerAgent =
  res
    "UpdateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse.proto"
    ecsService
    (Proxy :: Proxy UpdateContainerAgent)

responseListContainerInstances :: ListContainerInstancesResponse -> TestTree
responseListContainerInstances =
  res
    "ListContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse.proto"
    ecsService
    (Proxy :: Proxy ListContainerInstances)

responseUpdateServicePrimaryTaskSet :: UpdateServicePrimaryTaskSetResponse -> TestTree
responseUpdateServicePrimaryTaskSet =
  res
    "UpdateServicePrimaryTaskSetResponse"
    "fixture/UpdateServicePrimaryTaskSetResponse.proto"
    ecsService
    (Proxy :: Proxy UpdateServicePrimaryTaskSet)

responseListTaskDefinitionFamilies :: ListTaskDefinitionFamiliesResponse -> TestTree
responseListTaskDefinitionFamilies =
  res
    "ListTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse.proto"
    ecsService
    (Proxy :: Proxy ListTaskDefinitionFamilies)

responseStartTask :: StartTaskResponse -> TestTree
responseStartTask =
  res
    "StartTaskResponse"
    "fixture/StartTaskResponse.proto"
    ecsService
    (Proxy :: Proxy StartTask)

responsePutClusterCapacityProviders :: PutClusterCapacityProvidersResponse -> TestTree
responsePutClusterCapacityProviders =
  res
    "PutClusterCapacityProvidersResponse"
    "fixture/PutClusterCapacityProvidersResponse.proto"
    ecsService
    (Proxy :: Proxy PutClusterCapacityProviders)

responsePutAccountSettingDefault :: PutAccountSettingDefaultResponse -> TestTree
responsePutAccountSettingDefault =
  res
    "PutAccountSettingDefaultResponse"
    "fixture/PutAccountSettingDefaultResponse.proto"
    ecsService
    (Proxy :: Proxy PutAccountSettingDefault)

responseListAttributes :: ListAttributesResponse -> TestTree
responseListAttributes =
  res
    "ListAttributesResponse"
    "fixture/ListAttributesResponse.proto"
    ecsService
    (Proxy :: Proxy ListAttributes)

responseDeregisterTaskDefinition :: DeregisterTaskDefinitionResponse -> TestTree
responseDeregisterTaskDefinition =
  res
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse.proto"
    ecsService
    (Proxy :: Proxy DeregisterTaskDefinition)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    ecsService
    (Proxy :: Proxy TagResource)

responseCreateTaskSet :: CreateTaskSetResponse -> TestTree
responseCreateTaskSet =
  res
    "CreateTaskSetResponse"
    "fixture/CreateTaskSetResponse.proto"
    ecsService
    (Proxy :: Proxy CreateTaskSet)

responseDescribeTasks :: DescribeTasksResponse -> TestTree
responseDescribeTasks =
  res
    "DescribeTasksResponse"
    "fixture/DescribeTasksResponse.proto"
    ecsService
    (Proxy :: Proxy DescribeTasks)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    ecsService
    (Proxy :: Proxy ListClusters)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    ecsService
    (Proxy :: Proxy UntagResource)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices =
  res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    ecsService
    (Proxy :: Proxy DescribeServices)

responseDeregisterContainerInstance :: DeregisterContainerInstanceResponse -> TestTree
responseDeregisterContainerInstance =
  res
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse.proto"
    ecsService
    (Proxy :: Proxy DeregisterContainerInstance)

responseUpdateClusterSettings :: UpdateClusterSettingsResponse -> TestTree
responseUpdateClusterSettings =
  res
    "UpdateClusterSettingsResponse"
    "fixture/UpdateClusterSettingsResponse.proto"
    ecsService
    (Proxy :: Proxy UpdateClusterSettings)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes =
  res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    ecsService
    (Proxy :: Proxy DeleteAttributes)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes =
  res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    ecsService
    (Proxy :: Proxy PutAttributes)

responseListAccountSettings :: ListAccountSettingsResponse -> TestTree
responseListAccountSettings =
  res
    "ListAccountSettingsResponse"
    "fixture/ListAccountSettingsResponse.proto"
    ecsService
    (Proxy :: Proxy ListAccountSettings)

responseDeleteTaskSet :: DeleteTaskSetResponse -> TestTree
responseDeleteTaskSet =
  res
    "DeleteTaskSetResponse"
    "fixture/DeleteTaskSetResponse.proto"
    ecsService
    (Proxy :: Proxy DeleteTaskSet)

responseUpdateTaskSet :: UpdateTaskSetResponse -> TestTree
responseUpdateTaskSet =
  res
    "UpdateTaskSetResponse"
    "fixture/UpdateTaskSetResponse.proto"
    ecsService
    (Proxy :: Proxy UpdateTaskSet)

responseCreateCapacityProvider :: CreateCapacityProviderResponse -> TestTree
responseCreateCapacityProvider =
  res
    "CreateCapacityProviderResponse"
    "fixture/CreateCapacityProviderResponse.proto"
    ecsService
    (Proxy :: Proxy CreateCapacityProvider)

responseDescribeTaskSets :: DescribeTaskSetsResponse -> TestTree
responseDescribeTaskSets =
  res
    "DescribeTaskSetsResponse"
    "fixture/DescribeTaskSetsResponse.proto"
    ecsService
    (Proxy :: Proxy DescribeTaskSets)

responseRegisterTaskDefinition :: RegisterTaskDefinitionResponse -> TestTree
responseRegisterTaskDefinition =
  res
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse.proto"
    ecsService
    (Proxy :: Proxy RegisterTaskDefinition)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    ecsService
    (Proxy :: Proxy CreateService)
