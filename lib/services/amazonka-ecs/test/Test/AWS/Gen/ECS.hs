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

import qualified Data.Proxy as Proxy
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
--             newListServices
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestUpdateService $
--             newUpdateService
--
--         , requestDiscoverPollEndpoint $
--             newDiscoverPollEndpoint
--
--         , requestSubmitAttachmentStateChanges $
--             newSubmitAttachmentStateChanges
--
--         , requestSubmitContainerStateChange $
--             newSubmitContainerStateChange
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStopTask $
--             newStopTask
--
--         , requestDescribeTaskDefinition $
--             newDescribeTaskDefinition
--
--         , requestSubmitTaskStateChange $
--             newSubmitTaskStateChange
--
--         , requestDescribeContainerInstances $
--             newDescribeContainerInstances
--
--         , requestDescribeCapacityProviders $
--             newDescribeCapacityProviders
--
--         , requestUpdateContainerInstancesState $
--             newUpdateContainerInstancesState
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestPutAccountSetting $
--             newPutAccountSetting
--
--         , requestDeleteAccountSetting $
--             newDeleteAccountSetting
--
--         , requestListTaskDefinitions $
--             newListTaskDefinitions
--
--         , requestRunTask $
--             newRunTask
--
--         , requestDeleteCapacityProvider $
--             newDeleteCapacityProvider
--
--         , requestListTasks $
--             newListTasks
--
--         , requestUpdateCapacityProvider $
--             newUpdateCapacityProvider
--
--         , requestRegisterContainerInstance $
--             newRegisterContainerInstance
--
--         , requestUpdateContainerAgent $
--             newUpdateContainerAgent
--
--         , requestListContainerInstances $
--             newListContainerInstances
--
--         , requestUpdateServicePrimaryTaskSet $
--             newUpdateServicePrimaryTaskSet
--
--         , requestListTaskDefinitionFamilies $
--             newListTaskDefinitionFamilies
--
--         , requestStartTask $
--             newStartTask
--
--         , requestPutClusterCapacityProviders $
--             newPutClusterCapacityProviders
--
--         , requestPutAccountSettingDefault $
--             newPutAccountSettingDefault
--
--         , requestListAttributes $
--             newListAttributes
--
--         , requestExecuteCommand $
--             newExecuteCommand
--
--         , requestDeregisterTaskDefinition $
--             newDeregisterTaskDefinition
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateTaskSet $
--             newCreateTaskSet
--
--         , requestDescribeTasks $
--             newDescribeTasks
--
--         , requestListClusters $
--             newListClusters
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeServices $
--             newDescribeServices
--
--         , requestDeregisterContainerInstance $
--             newDeregisterContainerInstance
--
--         , requestUpdateClusterSettings $
--             newUpdateClusterSettings
--
--         , requestDeleteAttributes $
--             newDeleteAttributes
--
--         , requestPutAttributes $
--             newPutAttributes
--
--         , requestListAccountSettings $
--             newListAccountSettings
--
--         , requestDeleteTaskSet $
--             newDeleteTaskSet
--
--         , requestUpdateTaskSet $
--             newUpdateTaskSet
--
--         , requestCreateCapacityProvider $
--             newCreateCapacityProvider
--
--         , requestDescribeTaskSets $
--             newDescribeTaskSets
--
--         , requestRegisterTaskDefinition $
--             newRegisterTaskDefinition
--
--         , requestCreateService $
--             newCreateService
--
--           ]

--     , testGroup "response"
--         [ responseListServices $
--             newListServicesResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--         , responseDiscoverPollEndpoint $
--             newDiscoverPollEndpointResponse
--
--         , responseSubmitAttachmentStateChanges $
--             newSubmitAttachmentStateChangesResponse
--
--         , responseSubmitContainerStateChange $
--             newSubmitContainerStateChangeResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStopTask $
--             newStopTaskResponse
--
--         , responseDescribeTaskDefinition $
--             newDescribeTaskDefinitionResponse
--
--         , responseSubmitTaskStateChange $
--             newSubmitTaskStateChangeResponse
--
--         , responseDescribeContainerInstances $
--             newDescribeContainerInstancesResponse
--
--         , responseDescribeCapacityProviders $
--             newDescribeCapacityProvidersResponse
--
--         , responseUpdateContainerInstancesState $
--             newUpdateContainerInstancesStateResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responsePutAccountSetting $
--             newPutAccountSettingResponse
--
--         , responseDeleteAccountSetting $
--             newDeleteAccountSettingResponse
--
--         , responseListTaskDefinitions $
--             newListTaskDefinitionsResponse
--
--         , responseRunTask $
--             newRunTaskResponse
--
--         , responseDeleteCapacityProvider $
--             newDeleteCapacityProviderResponse
--
--         , responseListTasks $
--             newListTasksResponse
--
--         , responseUpdateCapacityProvider $
--             newUpdateCapacityProviderResponse
--
--         , responseRegisterContainerInstance $
--             newRegisterContainerInstanceResponse
--
--         , responseUpdateContainerAgent $
--             newUpdateContainerAgentResponse
--
--         , responseListContainerInstances $
--             newListContainerInstancesResponse
--
--         , responseUpdateServicePrimaryTaskSet $
--             newUpdateServicePrimaryTaskSetResponse
--
--         , responseListTaskDefinitionFamilies $
--             newListTaskDefinitionFamiliesResponse
--
--         , responseStartTask $
--             newStartTaskResponse
--
--         , responsePutClusterCapacityProviders $
--             newPutClusterCapacityProvidersResponse
--
--         , responsePutAccountSettingDefault $
--             newPutAccountSettingDefaultResponse
--
--         , responseListAttributes $
--             newListAttributesResponse
--
--         , responseExecuteCommand $
--             newExecuteCommandResponse
--
--         , responseDeregisterTaskDefinition $
--             newDeregisterTaskDefinitionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateTaskSet $
--             newCreateTaskSetResponse
--
--         , responseDescribeTasks $
--             newDescribeTasksResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeServices $
--             newDescribeServicesResponse
--
--         , responseDeregisterContainerInstance $
--             newDeregisterContainerInstanceResponse
--
--         , responseUpdateClusterSettings $
--             newUpdateClusterSettingsResponse
--
--         , responseDeleteAttributes $
--             newDeleteAttributesResponse
--
--         , responsePutAttributes $
--             newPutAttributesResponse
--
--         , responseListAccountSettings $
--             newListAccountSettingsResponse
--
--         , responseDeleteTaskSet $
--             newDeleteTaskSetResponse
--
--         , responseUpdateTaskSet $
--             newUpdateTaskSetResponse
--
--         , responseCreateCapacityProvider $
--             newCreateCapacityProviderResponse
--
--         , responseDescribeTaskSets $
--             newDescribeTaskSetsResponse
--
--         , responseRegisterTaskDefinition $
--             newRegisterTaskDefinitionResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
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

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

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

requestExecuteCommand :: ExecuteCommand -> TestTree
requestExecuteCommand =
  req
    "ExecuteCommand"
    "fixture/ExecuteCommand.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServices)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusters)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteService)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateService)

responseDiscoverPollEndpoint :: DiscoverPollEndpointResponse -> TestTree
responseDiscoverPollEndpoint =
  res
    "DiscoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DiscoverPollEndpoint)

responseSubmitAttachmentStateChanges :: SubmitAttachmentStateChangesResponse -> TestTree
responseSubmitAttachmentStateChanges =
  res
    "SubmitAttachmentStateChangesResponse"
    "fixture/SubmitAttachmentStateChangesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubmitAttachmentStateChanges)

responseSubmitContainerStateChange :: SubmitContainerStateChangeResponse -> TestTree
responseSubmitContainerStateChange =
  res
    "SubmitContainerStateChangeResponse"
    "fixture/SubmitContainerStateChangeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubmitContainerStateChange)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStopTask :: StopTaskResponse -> TestTree
responseStopTask =
  res
    "StopTaskResponse"
    "fixture/StopTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTask)

responseDescribeTaskDefinition :: DescribeTaskDefinitionResponse -> TestTree
responseDescribeTaskDefinition =
  res
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTaskDefinition)

responseSubmitTaskStateChange :: SubmitTaskStateChangeResponse -> TestTree
responseSubmitTaskStateChange =
  res
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubmitTaskStateChange)

responseDescribeContainerInstances :: DescribeContainerInstancesResponse -> TestTree
responseDescribeContainerInstances =
  res
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContainerInstances)

responseDescribeCapacityProviders :: DescribeCapacityProvidersResponse -> TestTree
responseDescribeCapacityProviders =
  res
    "DescribeCapacityProvidersResponse"
    "fixture/DescribeCapacityProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCapacityProviders)

responseUpdateContainerInstancesState :: UpdateContainerInstancesStateResponse -> TestTree
responseUpdateContainerInstancesState =
  res
    "UpdateContainerInstancesStateResponse"
    "fixture/UpdateContainerInstancesStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContainerInstancesState)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCluster)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responsePutAccountSetting :: PutAccountSettingResponse -> TestTree
responsePutAccountSetting =
  res
    "PutAccountSettingResponse"
    "fixture/PutAccountSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountSetting)

responseDeleteAccountSetting :: DeleteAccountSettingResponse -> TestTree
responseDeleteAccountSetting =
  res
    "DeleteAccountSettingResponse"
    "fixture/DeleteAccountSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountSetting)

responseListTaskDefinitions :: ListTaskDefinitionsResponse -> TestTree
responseListTaskDefinitions =
  res
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTaskDefinitions)

responseRunTask :: RunTaskResponse -> TestTree
responseRunTask =
  res
    "RunTaskResponse"
    "fixture/RunTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunTask)

responseDeleteCapacityProvider :: DeleteCapacityProviderResponse -> TestTree
responseDeleteCapacityProvider =
  res
    "DeleteCapacityProviderResponse"
    "fixture/DeleteCapacityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCapacityProvider)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks =
  res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTasks)

responseUpdateCapacityProvider :: UpdateCapacityProviderResponse -> TestTree
responseUpdateCapacityProvider =
  res
    "UpdateCapacityProviderResponse"
    "fixture/UpdateCapacityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCapacityProvider)

responseRegisterContainerInstance :: RegisterContainerInstanceResponse -> TestTree
responseRegisterContainerInstance =
  res
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterContainerInstance)

responseUpdateContainerAgent :: UpdateContainerAgentResponse -> TestTree
responseUpdateContainerAgent =
  res
    "UpdateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContainerAgent)

responseListContainerInstances :: ListContainerInstancesResponse -> TestTree
responseListContainerInstances =
  res
    "ListContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContainerInstances)

responseUpdateServicePrimaryTaskSet :: UpdateServicePrimaryTaskSetResponse -> TestTree
responseUpdateServicePrimaryTaskSet =
  res
    "UpdateServicePrimaryTaskSetResponse"
    "fixture/UpdateServicePrimaryTaskSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServicePrimaryTaskSet)

responseListTaskDefinitionFamilies :: ListTaskDefinitionFamiliesResponse -> TestTree
responseListTaskDefinitionFamilies =
  res
    "ListTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTaskDefinitionFamilies)

responseStartTask :: StartTaskResponse -> TestTree
responseStartTask =
  res
    "StartTaskResponse"
    "fixture/StartTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTask)

responsePutClusterCapacityProviders :: PutClusterCapacityProvidersResponse -> TestTree
responsePutClusterCapacityProviders =
  res
    "PutClusterCapacityProvidersResponse"
    "fixture/PutClusterCapacityProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutClusterCapacityProviders)

responsePutAccountSettingDefault :: PutAccountSettingDefaultResponse -> TestTree
responsePutAccountSettingDefault =
  res
    "PutAccountSettingDefaultResponse"
    "fixture/PutAccountSettingDefaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountSettingDefault)

responseListAttributes :: ListAttributesResponse -> TestTree
responseListAttributes =
  res
    "ListAttributesResponse"
    "fixture/ListAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttributes)

responseExecuteCommand :: ExecuteCommandResponse -> TestTree
responseExecuteCommand =
  res
    "ExecuteCommandResponse"
    "fixture/ExecuteCommandResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteCommand)

responseDeregisterTaskDefinition :: DeregisterTaskDefinitionResponse -> TestTree
responseDeregisterTaskDefinition =
  res
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTaskDefinition)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateTaskSet :: CreateTaskSetResponse -> TestTree
responseCreateTaskSet =
  res
    "CreateTaskSetResponse"
    "fixture/CreateTaskSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTaskSet)

responseDescribeTasks :: DescribeTasksResponse -> TestTree
responseDescribeTasks =
  res
    "DescribeTasksResponse"
    "fixture/DescribeTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTasks)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices =
  res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServices)

responseDeregisterContainerInstance :: DeregisterContainerInstanceResponse -> TestTree
responseDeregisterContainerInstance =
  res
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterContainerInstance)

responseUpdateClusterSettings :: UpdateClusterSettingsResponse -> TestTree
responseUpdateClusterSettings =
  res
    "UpdateClusterSettingsResponse"
    "fixture/UpdateClusterSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClusterSettings)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes =
  res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAttributes)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes =
  res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAttributes)

responseListAccountSettings :: ListAccountSettingsResponse -> TestTree
responseListAccountSettings =
  res
    "ListAccountSettingsResponse"
    "fixture/ListAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountSettings)

responseDeleteTaskSet :: DeleteTaskSetResponse -> TestTree
responseDeleteTaskSet =
  res
    "DeleteTaskSetResponse"
    "fixture/DeleteTaskSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTaskSet)

responseUpdateTaskSet :: UpdateTaskSetResponse -> TestTree
responseUpdateTaskSet =
  res
    "UpdateTaskSetResponse"
    "fixture/UpdateTaskSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTaskSet)

responseCreateCapacityProvider :: CreateCapacityProviderResponse -> TestTree
responseCreateCapacityProvider =
  res
    "CreateCapacityProviderResponse"
    "fixture/CreateCapacityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCapacityProvider)

responseDescribeTaskSets :: DescribeTaskSetsResponse -> TestTree
responseDescribeTaskSets =
  res
    "DescribeTaskSetsResponse"
    "fixture/DescribeTaskSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTaskSets)

responseRegisterTaskDefinition :: RegisterTaskDefinitionResponse -> TestTree
responseRegisterTaskDefinition =
  res
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTaskDefinition)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)
