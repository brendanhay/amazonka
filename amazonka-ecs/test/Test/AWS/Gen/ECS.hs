{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ECS
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestSubmitAttachmentStateChanges $
--             newSubmitAttachmentStateChanges
--
--         , requestRegisterContainerInstance $
--             newRegisterContainerInstance
--
--         , requestDiscoverPollEndpoint $
--             newDiscoverPollEndpoint
--
--         , requestUpdateServicePrimaryTaskSet $
--             newUpdateServicePrimaryTaskSet
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestListServices $
--             newListServices
--
--         , requestRunTask $
--             newRunTask
--
--         , requestListTasks $
--             newListTasks
--
--         , requestCreateService $
--             newCreateService
--
--         , requestPutAccountSetting $
--             newPutAccountSetting
--
--         , requestDeleteAttributes $
--             newDeleteAttributes
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateTaskSet $
--             newCreateTaskSet
--
--         , requestDescribeTasks $
--             newDescribeTasks
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeContainerInstances $
--             newDescribeContainerInstances
--
--         , requestTagResource $
--             newTagResource
--
--         , requestPutAccountSettingDefault $
--             newPutAccountSettingDefault
--
--         , requestListAttributes $
--             newListAttributes
--
--         , requestSubmitContainerStateChange $
--             newSubmitContainerStateChange
--
--         , requestListContainerInstances $
--             newListContainerInstances
--
--         , requestUpdateContainerAgent $
--             newUpdateContainerAgent
--
--         , requestUpdateCapacityProvider $
--             newUpdateCapacityProvider
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestUpdateService $
--             newUpdateService
--
--         , requestDeleteCapacityProvider $
--             newDeleteCapacityProvider
--
--         , requestDescribeTaskSets $
--             newDescribeTaskSets
--
--         , requestListTaskDefinitions $
--             newListTaskDefinitions
--
--         , requestCreateCapacityProvider $
--             newCreateCapacityProvider
--
--         , requestRegisterTaskDefinition $
--             newRegisterTaskDefinition
--
--         , requestDeleteTaskSet $
--             newDeleteTaskSet
--
--         , requestUpdateClusterSettings $
--             newUpdateClusterSettings
--
--         , requestUpdateTaskSet $
--             newUpdateTaskSet
--
--         , requestDeregisterContainerInstance $
--             newDeregisterContainerInstance
--
--         , requestPutAttributes $
--             newPutAttributes
--
--         , requestDeleteAccountSetting $
--             newDeleteAccountSetting
--
--         , requestListAccountSettings $
--             newListAccountSettings
--
--         , requestDescribeServices $
--             newDescribeServices
--
--         , requestDescribeCapacityProviders $
--             newDescribeCapacityProviders
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestListClusters $
--             newListClusters
--
--         , requestUpdateContainerInstancesState $
--             newUpdateContainerInstancesState
--
--         , requestSubmitTaskStateChange $
--             newSubmitTaskStateChange
--
--         , requestDeregisterTaskDefinition $
--             newDeregisterTaskDefinition
--
--         , requestStopTask $
--             newStopTask
--
--         , requestPutClusterCapacityProviders $
--             newPutClusterCapacityProviders
--
--         , requestDescribeTaskDefinition $
--             newDescribeTaskDefinition
--
--         , requestStartTask $
--             newStartTask
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTaskDefinitionFamilies $
--             newListTaskDefinitionFamilies
--
--           ]

--     , testGroup "response"
--         [ responseSubmitAttachmentStateChanges $
--             newSubmitAttachmentStateChangesResponse
--
--         , responseRegisterContainerInstance $
--             newRegisterContainerInstanceResponse
--
--         , responseDiscoverPollEndpoint $
--             newDiscoverPollEndpointResponse
--
--         , responseUpdateServicePrimaryTaskSet $
--             newUpdateServicePrimaryTaskSetResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseListServices $
--             newListServicesResponse
--
--         , responseRunTask $
--             newRunTaskResponse
--
--         , responseListTasks $
--             newListTasksResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responsePutAccountSetting $
--             newPutAccountSettingResponse
--
--         , responseDeleteAttributes $
--             newDeleteAttributesResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateTaskSet $
--             newCreateTaskSetResponse
--
--         , responseDescribeTasks $
--             newDescribeTasksResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeContainerInstances $
--             newDescribeContainerInstancesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responsePutAccountSettingDefault $
--             newPutAccountSettingDefaultResponse
--
--         , responseListAttributes $
--             newListAttributesResponse
--
--         , responseSubmitContainerStateChange $
--             newSubmitContainerStateChangeResponse
--
--         , responseListContainerInstances $
--             newListContainerInstancesResponse
--
--         , responseUpdateContainerAgent $
--             newUpdateContainerAgentResponse
--
--         , responseUpdateCapacityProvider $
--             newUpdateCapacityProviderResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--         , responseDeleteCapacityProvider $
--             newDeleteCapacityProviderResponse
--
--         , responseDescribeTaskSets $
--             newDescribeTaskSetsResponse
--
--         , responseListTaskDefinitions $
--             newListTaskDefinitionsResponse
--
--         , responseCreateCapacityProvider $
--             newCreateCapacityProviderResponse
--
--         , responseRegisterTaskDefinition $
--             newRegisterTaskDefinitionResponse
--
--         , responseDeleteTaskSet $
--             newDeleteTaskSetResponse
--
--         , responseUpdateClusterSettings $
--             newUpdateClusterSettingsResponse
--
--         , responseUpdateTaskSet $
--             newUpdateTaskSetResponse
--
--         , responseDeregisterContainerInstance $
--             newDeregisterContainerInstanceResponse
--
--         , responsePutAttributes $
--             newPutAttributesResponse
--
--         , responseDeleteAccountSetting $
--             newDeleteAccountSettingResponse
--
--         , responseListAccountSettings $
--             newListAccountSettingsResponse
--
--         , responseDescribeServices $
--             newDescribeServicesResponse
--
--         , responseDescribeCapacityProviders $
--             newDescribeCapacityProvidersResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseUpdateContainerInstancesState $
--             newUpdateContainerInstancesStateResponse
--
--         , responseSubmitTaskStateChange $
--             newSubmitTaskStateChangeResponse
--
--         , responseDeregisterTaskDefinition $
--             newDeregisterTaskDefinitionResponse
--
--         , responseStopTask $
--             newStopTaskResponse
--
--         , responsePutClusterCapacityProviders $
--             newPutClusterCapacityProvidersResponse
--
--         , responseDescribeTaskDefinition $
--             newDescribeTaskDefinitionResponse
--
--         , responseStartTask $
--             newStartTaskResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTaskDefinitionFamilies $
--             newListTaskDefinitionFamiliesResponse
--
--           ]
--     ]

-- Requests

requestSubmitAttachmentStateChanges :: SubmitAttachmentStateChanges -> TestTree
requestSubmitAttachmentStateChanges =
  req
    "SubmitAttachmentStateChanges"
    "fixture/SubmitAttachmentStateChanges.yaml"

requestRegisterContainerInstance :: RegisterContainerInstance -> TestTree
requestRegisterContainerInstance =
  req
    "RegisterContainerInstance"
    "fixture/RegisterContainerInstance.yaml"

requestDiscoverPollEndpoint :: DiscoverPollEndpoint -> TestTree
requestDiscoverPollEndpoint =
  req
    "DiscoverPollEndpoint"
    "fixture/DiscoverPollEndpoint.yaml"

requestUpdateServicePrimaryTaskSet :: UpdateServicePrimaryTaskSet -> TestTree
requestUpdateServicePrimaryTaskSet =
  req
    "UpdateServicePrimaryTaskSet"
    "fixture/UpdateServicePrimaryTaskSet.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestRunTask :: RunTask -> TestTree
requestRunTask =
  req
    "RunTask"
    "fixture/RunTask.yaml"

requestListTasks :: ListTasks -> TestTree
requestListTasks =
  req
    "ListTasks"
    "fixture/ListTasks.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

requestPutAccountSetting :: PutAccountSetting -> TestTree
requestPutAccountSetting =
  req
    "PutAccountSetting"
    "fixture/PutAccountSetting.yaml"

requestDeleteAttributes :: DeleteAttributes -> TestTree
requestDeleteAttributes =
  req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeContainerInstances :: DescribeContainerInstances -> TestTree
requestDescribeContainerInstances =
  req
    "DescribeContainerInstances"
    "fixture/DescribeContainerInstances.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestSubmitContainerStateChange :: SubmitContainerStateChange -> TestTree
requestSubmitContainerStateChange =
  req
    "SubmitContainerStateChange"
    "fixture/SubmitContainerStateChange.yaml"

requestListContainerInstances :: ListContainerInstances -> TestTree
requestListContainerInstances =
  req
    "ListContainerInstances"
    "fixture/ListContainerInstances.yaml"

requestUpdateContainerAgent :: UpdateContainerAgent -> TestTree
requestUpdateContainerAgent =
  req
    "UpdateContainerAgent"
    "fixture/UpdateContainerAgent.yaml"

requestUpdateCapacityProvider :: UpdateCapacityProvider -> TestTree
requestUpdateCapacityProvider =
  req
    "UpdateCapacityProvider"
    "fixture/UpdateCapacityProvider.yaml"

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

requestDeleteCapacityProvider :: DeleteCapacityProvider -> TestTree
requestDeleteCapacityProvider =
  req
    "DeleteCapacityProvider"
    "fixture/DeleteCapacityProvider.yaml"

requestDescribeTaskSets :: DescribeTaskSets -> TestTree
requestDescribeTaskSets =
  req
    "DescribeTaskSets"
    "fixture/DescribeTaskSets.yaml"

requestListTaskDefinitions :: ListTaskDefinitions -> TestTree
requestListTaskDefinitions =
  req
    "ListTaskDefinitions"
    "fixture/ListTaskDefinitions.yaml"

requestCreateCapacityProvider :: CreateCapacityProvider -> TestTree
requestCreateCapacityProvider =
  req
    "CreateCapacityProvider"
    "fixture/CreateCapacityProvider.yaml"

requestRegisterTaskDefinition :: RegisterTaskDefinition -> TestTree
requestRegisterTaskDefinition =
  req
    "RegisterTaskDefinition"
    "fixture/RegisterTaskDefinition.yaml"

requestDeleteTaskSet :: DeleteTaskSet -> TestTree
requestDeleteTaskSet =
  req
    "DeleteTaskSet"
    "fixture/DeleteTaskSet.yaml"

requestUpdateClusterSettings :: UpdateClusterSettings -> TestTree
requestUpdateClusterSettings =
  req
    "UpdateClusterSettings"
    "fixture/UpdateClusterSettings.yaml"

requestUpdateTaskSet :: UpdateTaskSet -> TestTree
requestUpdateTaskSet =
  req
    "UpdateTaskSet"
    "fixture/UpdateTaskSet.yaml"

requestDeregisterContainerInstance :: DeregisterContainerInstance -> TestTree
requestDeregisterContainerInstance =
  req
    "DeregisterContainerInstance"
    "fixture/DeregisterContainerInstance.yaml"

requestPutAttributes :: PutAttributes -> TestTree
requestPutAttributes =
  req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

requestDeleteAccountSetting :: DeleteAccountSetting -> TestTree
requestDeleteAccountSetting =
  req
    "DeleteAccountSetting"
    "fixture/DeleteAccountSetting.yaml"

requestListAccountSettings :: ListAccountSettings -> TestTree
requestListAccountSettings =
  req
    "ListAccountSettings"
    "fixture/ListAccountSettings.yaml"

requestDescribeServices :: DescribeServices -> TestTree
requestDescribeServices =
  req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

requestDescribeCapacityProviders :: DescribeCapacityProviders -> TestTree
requestDescribeCapacityProviders =
  req
    "DescribeCapacityProviders"
    "fixture/DescribeCapacityProviders.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestUpdateContainerInstancesState :: UpdateContainerInstancesState -> TestTree
requestUpdateContainerInstancesState =
  req
    "UpdateContainerInstancesState"
    "fixture/UpdateContainerInstancesState.yaml"

requestSubmitTaskStateChange :: SubmitTaskStateChange -> TestTree
requestSubmitTaskStateChange =
  req
    "SubmitTaskStateChange"
    "fixture/SubmitTaskStateChange.yaml"

requestDeregisterTaskDefinition :: DeregisterTaskDefinition -> TestTree
requestDeregisterTaskDefinition =
  req
    "DeregisterTaskDefinition"
    "fixture/DeregisterTaskDefinition.yaml"

requestStopTask :: StopTask -> TestTree
requestStopTask =
  req
    "StopTask"
    "fixture/StopTask.yaml"

requestPutClusterCapacityProviders :: PutClusterCapacityProviders -> TestTree
requestPutClusterCapacityProviders =
  req
    "PutClusterCapacityProviders"
    "fixture/PutClusterCapacityProviders.yaml"

requestDescribeTaskDefinition :: DescribeTaskDefinition -> TestTree
requestDescribeTaskDefinition =
  req
    "DescribeTaskDefinition"
    "fixture/DescribeTaskDefinition.yaml"

requestStartTask :: StartTask -> TestTree
requestStartTask =
  req
    "StartTask"
    "fixture/StartTask.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTaskDefinitionFamilies :: ListTaskDefinitionFamilies -> TestTree
requestListTaskDefinitionFamilies =
  req
    "ListTaskDefinitionFamilies"
    "fixture/ListTaskDefinitionFamilies.yaml"

-- Responses

responseSubmitAttachmentStateChanges :: SubmitAttachmentStateChangesResponse -> TestTree
responseSubmitAttachmentStateChanges =
  res
    "SubmitAttachmentStateChangesResponse"
    "fixture/SubmitAttachmentStateChangesResponse.proto"
    defaultService
    (Proxy :: Proxy SubmitAttachmentStateChanges)

responseRegisterContainerInstance :: RegisterContainerInstanceResponse -> TestTree
responseRegisterContainerInstance =
  res
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterContainerInstance)

responseDiscoverPollEndpoint :: DiscoverPollEndpointResponse -> TestTree
responseDiscoverPollEndpoint =
  res
    "DiscoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DiscoverPollEndpoint)

responseUpdateServicePrimaryTaskSet :: UpdateServicePrimaryTaskSetResponse -> TestTree
responseUpdateServicePrimaryTaskSet =
  res
    "UpdateServicePrimaryTaskSetResponse"
    "fixture/UpdateServicePrimaryTaskSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServicePrimaryTaskSet)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusters)

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListServices)

responseRunTask :: RunTaskResponse -> TestTree
responseRunTask =
  res
    "RunTaskResponse"
    "fixture/RunTaskResponse.proto"
    defaultService
    (Proxy :: Proxy RunTask)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks =
  res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListTasks)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateService)

responsePutAccountSetting :: PutAccountSettingResponse -> TestTree
responsePutAccountSetting =
  res
    "PutAccountSettingResponse"
    "fixture/PutAccountSettingResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountSetting)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes =
  res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAttributes)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCluster)

responseCreateTaskSet :: CreateTaskSetResponse -> TestTree
responseCreateTaskSet =
  res
    "CreateTaskSetResponse"
    "fixture/CreateTaskSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTaskSet)

responseDescribeTasks :: DescribeTasksResponse -> TestTree
responseDescribeTasks =
  res
    "DescribeTasksResponse"
    "fixture/DescribeTasksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTasks)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeContainerInstances :: DescribeContainerInstancesResponse -> TestTree
responseDescribeContainerInstances =
  res
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContainerInstances)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responsePutAccountSettingDefault :: PutAccountSettingDefaultResponse -> TestTree
responsePutAccountSettingDefault =
  res
    "PutAccountSettingDefaultResponse"
    "fixture/PutAccountSettingDefaultResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountSettingDefault)

responseListAttributes :: ListAttributesResponse -> TestTree
responseListAttributes =
  res
    "ListAttributesResponse"
    "fixture/ListAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttributes)

responseSubmitContainerStateChange :: SubmitContainerStateChangeResponse -> TestTree
responseSubmitContainerStateChange =
  res
    "SubmitContainerStateChangeResponse"
    "fixture/SubmitContainerStateChangeResponse.proto"
    defaultService
    (Proxy :: Proxy SubmitContainerStateChange)

responseListContainerInstances :: ListContainerInstancesResponse -> TestTree
responseListContainerInstances =
  res
    "ListContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListContainerInstances)

responseUpdateContainerAgent :: UpdateContainerAgentResponse -> TestTree
responseUpdateContainerAgent =
  res
    "UpdateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContainerAgent)

responseUpdateCapacityProvider :: UpdateCapacityProviderResponse -> TestTree
responseUpdateCapacityProvider =
  res
    "UpdateCapacityProviderResponse"
    "fixture/UpdateCapacityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCapacityProvider)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteService)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateService)

responseDeleteCapacityProvider :: DeleteCapacityProviderResponse -> TestTree
responseDeleteCapacityProvider =
  res
    "DeleteCapacityProviderResponse"
    "fixture/DeleteCapacityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCapacityProvider)

responseDescribeTaskSets :: DescribeTaskSetsResponse -> TestTree
responseDescribeTaskSets =
  res
    "DescribeTaskSetsResponse"
    "fixture/DescribeTaskSetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTaskSets)

responseListTaskDefinitions :: ListTaskDefinitionsResponse -> TestTree
responseListTaskDefinitions =
  res
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTaskDefinitions)

responseCreateCapacityProvider :: CreateCapacityProviderResponse -> TestTree
responseCreateCapacityProvider =
  res
    "CreateCapacityProviderResponse"
    "fixture/CreateCapacityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCapacityProvider)

responseRegisterTaskDefinition :: RegisterTaskDefinitionResponse -> TestTree
responseRegisterTaskDefinition =
  res
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTaskDefinition)

responseDeleteTaskSet :: DeleteTaskSetResponse -> TestTree
responseDeleteTaskSet =
  res
    "DeleteTaskSetResponse"
    "fixture/DeleteTaskSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTaskSet)

responseUpdateClusterSettings :: UpdateClusterSettingsResponse -> TestTree
responseUpdateClusterSettings =
  res
    "UpdateClusterSettingsResponse"
    "fixture/UpdateClusterSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateClusterSettings)

responseUpdateTaskSet :: UpdateTaskSetResponse -> TestTree
responseUpdateTaskSet =
  res
    "UpdateTaskSetResponse"
    "fixture/UpdateTaskSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTaskSet)

responseDeregisterContainerInstance :: DeregisterContainerInstanceResponse -> TestTree
responseDeregisterContainerInstance =
  res
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterContainerInstance)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes =
  res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAttributes)

responseDeleteAccountSetting :: DeleteAccountSettingResponse -> TestTree
responseDeleteAccountSetting =
  res
    "DeleteAccountSettingResponse"
    "fixture/DeleteAccountSettingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccountSetting)

responseListAccountSettings :: ListAccountSettingsResponse -> TestTree
responseListAccountSettings =
  res
    "ListAccountSettingsResponse"
    "fixture/ListAccountSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccountSettings)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices =
  res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServices)

responseDescribeCapacityProviders :: DescribeCapacityProvidersResponse -> TestTree
responseDescribeCapacityProviders =
  res
    "DescribeCapacityProvidersResponse"
    "fixture/DescribeCapacityProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCapacityProviders)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy :: Proxy ListClusters)

responseUpdateContainerInstancesState :: UpdateContainerInstancesStateResponse -> TestTree
responseUpdateContainerInstancesState =
  res
    "UpdateContainerInstancesStateResponse"
    "fixture/UpdateContainerInstancesStateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContainerInstancesState)

responseSubmitTaskStateChange :: SubmitTaskStateChangeResponse -> TestTree
responseSubmitTaskStateChange =
  res
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse.proto"
    defaultService
    (Proxy :: Proxy SubmitTaskStateChange)

responseDeregisterTaskDefinition :: DeregisterTaskDefinitionResponse -> TestTree
responseDeregisterTaskDefinition =
  res
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTaskDefinition)

responseStopTask :: StopTaskResponse -> TestTree
responseStopTask =
  res
    "StopTaskResponse"
    "fixture/StopTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StopTask)

responsePutClusterCapacityProviders :: PutClusterCapacityProvidersResponse -> TestTree
responsePutClusterCapacityProviders =
  res
    "PutClusterCapacityProvidersResponse"
    "fixture/PutClusterCapacityProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy PutClusterCapacityProviders)

responseDescribeTaskDefinition :: DescribeTaskDefinitionResponse -> TestTree
responseDescribeTaskDefinition =
  res
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTaskDefinition)

responseStartTask :: StartTaskResponse -> TestTree
responseStartTask =
  res
    "StartTaskResponse"
    "fixture/StartTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartTask)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseListTaskDefinitionFamilies :: ListTaskDefinitionFamiliesResponse -> TestTree
responseListTaskDefinitionFamilies =
  res
    "ListTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTaskDefinitionFamilies)
