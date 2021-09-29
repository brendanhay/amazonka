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
--         [ requestDiscoverPollEndpoint $
--             newDiscoverPollEndpoint
--
--         , requestUpdateServicePrimaryTaskSet $
--             newUpdateServicePrimaryTaskSet
--
--         , requestRegisterContainerInstance $
--             newRegisterContainerInstance
--
--         , requestSubmitAttachmentStateChanges $
--             newSubmitAttachmentStateChanges
--
--         , requestRunTask $
--             newRunTask
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestListTasks $
--             newListTasks
--
--         , requestListServices $
--             newListServices
--
--         , requestCreateService $
--             newCreateService
--
--         , requestDeleteAttributes $
--             newDeleteAttributes
--
--         , requestPutAccountSetting $
--             newPutAccountSetting
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
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeContainerInstances $
--             newDescribeContainerInstances
--
--         , requestListAttributes $
--             newListAttributes
--
--         , requestPutAccountSettingDefault $
--             newPutAccountSettingDefault
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
--         , requestDeleteCapacityProvider $
--             newDeleteCapacityProvider
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestUpdateService $
--             newUpdateService
--
--         , requestUpdateCapacityProvider $
--             newUpdateCapacityProvider
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
--         , requestListTaskDefinitions $
--             newListTaskDefinitions
--
--         , requestPutAttributes $
--             newPutAttributes
--
--         , requestDeleteTaskSet $
--             newDeleteTaskSet
--
--         , requestUpdateClusterSettings $
--             newUpdateClusterSettings
--
--         , requestDeregisterContainerInstance $
--             newDeregisterContainerInstance
--
--         , requestUpdateTaskSet $
--             newUpdateTaskSet
--
--         , requestDeleteAccountSetting $
--             newDeleteAccountSetting
--
--         , requestListAccountSettings $
--             newListAccountSettings
--
--         , requestUpdateContainerInstancesState $
--             newUpdateContainerInstancesState
--
--         , requestDescribeCapacityProviders $
--             newDescribeCapacityProviders
--
--         , requestListClusters $
--             newListClusters
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDescribeServices $
--             newDescribeServices
--
--         , requestSubmitTaskStateChange $
--             newSubmitTaskStateChange
--
--         , requestExecuteCommand $
--             newExecuteCommand
--
--         , requestDeregisterTaskDefinition $
--             newDeregisterTaskDefinition
--
--         , requestStartTask $
--             newStartTask
--
--         , requestDescribeTaskDefinition $
--             newDescribeTaskDefinition
--
--         , requestPutClusterCapacityProviders $
--             newPutClusterCapacityProviders
--
--         , requestStopTask $
--             newStopTask
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTaskDefinitionFamilies $
--             newListTaskDefinitionFamilies
--
--           ]

--     , testGroup "response"
--         [ responseDiscoverPollEndpoint $
--             newDiscoverPollEndpointResponse
--
--         , responseUpdateServicePrimaryTaskSet $
--             newUpdateServicePrimaryTaskSetResponse
--
--         , responseRegisterContainerInstance $
--             newRegisterContainerInstanceResponse
--
--         , responseSubmitAttachmentStateChanges $
--             newSubmitAttachmentStateChangesResponse
--
--         , responseRunTask $
--             newRunTaskResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseListTasks $
--             newListTasksResponse
--
--         , responseListServices $
--             newListServicesResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseDeleteAttributes $
--             newDeleteAttributesResponse
--
--         , responsePutAccountSetting $
--             newPutAccountSettingResponse
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
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeContainerInstances $
--             newDescribeContainerInstancesResponse
--
--         , responseListAttributes $
--             newListAttributesResponse
--
--         , responsePutAccountSettingDefault $
--             newPutAccountSettingDefaultResponse
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
--         , responseDeleteCapacityProvider $
--             newDeleteCapacityProviderResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--         , responseUpdateCapacityProvider $
--             newUpdateCapacityProviderResponse
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
--         , responseListTaskDefinitions $
--             newListTaskDefinitionsResponse
--
--         , responsePutAttributes $
--             newPutAttributesResponse
--
--         , responseDeleteTaskSet $
--             newDeleteTaskSetResponse
--
--         , responseUpdateClusterSettings $
--             newUpdateClusterSettingsResponse
--
--         , responseDeregisterContainerInstance $
--             newDeregisterContainerInstanceResponse
--
--         , responseUpdateTaskSet $
--             newUpdateTaskSetResponse
--
--         , responseDeleteAccountSetting $
--             newDeleteAccountSettingResponse
--
--         , responseListAccountSettings $
--             newListAccountSettingsResponse
--
--         , responseUpdateContainerInstancesState $
--             newUpdateContainerInstancesStateResponse
--
--         , responseDescribeCapacityProviders $
--             newDescribeCapacityProvidersResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDescribeServices $
--             newDescribeServicesResponse
--
--         , responseSubmitTaskStateChange $
--             newSubmitTaskStateChangeResponse
--
--         , responseExecuteCommand $
--             newExecuteCommandResponse
--
--         , responseDeregisterTaskDefinition $
--             newDeregisterTaskDefinitionResponse
--
--         , responseStartTask $
--             newStartTaskResponse
--
--         , responseDescribeTaskDefinition $
--             newDescribeTaskDefinitionResponse
--
--         , responsePutClusterCapacityProviders $
--             newPutClusterCapacityProvidersResponse
--
--         , responseStopTask $
--             newStopTaskResponse
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

requestRegisterContainerInstance :: RegisterContainerInstance -> TestTree
requestRegisterContainerInstance =
  req
    "RegisterContainerInstance"
    "fixture/RegisterContainerInstance.yaml"

requestSubmitAttachmentStateChanges :: SubmitAttachmentStateChanges -> TestTree
requestSubmitAttachmentStateChanges =
  req
    "SubmitAttachmentStateChanges"
    "fixture/SubmitAttachmentStateChanges.yaml"

requestRunTask :: RunTask -> TestTree
requestRunTask =
  req
    "RunTask"
    "fixture/RunTask.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestListTasks :: ListTasks -> TestTree
requestListTasks =
  req
    "ListTasks"
    "fixture/ListTasks.yaml"

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

requestDeleteAttributes :: DeleteAttributes -> TestTree
requestDeleteAttributes =
  req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

requestPutAccountSetting :: PutAccountSetting -> TestTree
requestPutAccountSetting =
  req
    "PutAccountSetting"
    "fixture/PutAccountSetting.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeContainerInstances :: DescribeContainerInstances -> TestTree
requestDescribeContainerInstances =
  req
    "DescribeContainerInstances"
    "fixture/DescribeContainerInstances.yaml"

requestListAttributes :: ListAttributes -> TestTree
requestListAttributes =
  req
    "ListAttributes"
    "fixture/ListAttributes.yaml"

requestPutAccountSettingDefault :: PutAccountSettingDefault -> TestTree
requestPutAccountSettingDefault =
  req
    "PutAccountSettingDefault"
    "fixture/PutAccountSettingDefault.yaml"

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

requestDeleteCapacityProvider :: DeleteCapacityProvider -> TestTree
requestDeleteCapacityProvider =
  req
    "DeleteCapacityProvider"
    "fixture/DeleteCapacityProvider.yaml"

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

requestUpdateCapacityProvider :: UpdateCapacityProvider -> TestTree
requestUpdateCapacityProvider =
  req
    "UpdateCapacityProvider"
    "fixture/UpdateCapacityProvider.yaml"

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

requestListTaskDefinitions :: ListTaskDefinitions -> TestTree
requestListTaskDefinitions =
  req
    "ListTaskDefinitions"
    "fixture/ListTaskDefinitions.yaml"

requestPutAttributes :: PutAttributes -> TestTree
requestPutAttributes =
  req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

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

requestDeregisterContainerInstance :: DeregisterContainerInstance -> TestTree
requestDeregisterContainerInstance =
  req
    "DeregisterContainerInstance"
    "fixture/DeregisterContainerInstance.yaml"

requestUpdateTaskSet :: UpdateTaskSet -> TestTree
requestUpdateTaskSet =
  req
    "UpdateTaskSet"
    "fixture/UpdateTaskSet.yaml"

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

requestUpdateContainerInstancesState :: UpdateContainerInstancesState -> TestTree
requestUpdateContainerInstancesState =
  req
    "UpdateContainerInstancesState"
    "fixture/UpdateContainerInstancesState.yaml"

requestDescribeCapacityProviders :: DescribeCapacityProviders -> TestTree
requestDescribeCapacityProviders =
  req
    "DescribeCapacityProviders"
    "fixture/DescribeCapacityProviders.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDescribeServices :: DescribeServices -> TestTree
requestDescribeServices =
  req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

requestSubmitTaskStateChange :: SubmitTaskStateChange -> TestTree
requestSubmitTaskStateChange =
  req
    "SubmitTaskStateChange"
    "fixture/SubmitTaskStateChange.yaml"

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

requestStartTask :: StartTask -> TestTree
requestStartTask =
  req
    "StartTask"
    "fixture/StartTask.yaml"

requestDescribeTaskDefinition :: DescribeTaskDefinition -> TestTree
requestDescribeTaskDefinition =
  req
    "DescribeTaskDefinition"
    "fixture/DescribeTaskDefinition.yaml"

requestPutClusterCapacityProviders :: PutClusterCapacityProviders -> TestTree
requestPutClusterCapacityProviders =
  req
    "PutClusterCapacityProviders"
    "fixture/PutClusterCapacityProviders.yaml"

requestStopTask :: StopTask -> TestTree
requestStopTask =
  req
    "StopTask"
    "fixture/StopTask.yaml"

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

responseRegisterContainerInstance :: RegisterContainerInstanceResponse -> TestTree
responseRegisterContainerInstance =
  res
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterContainerInstance)

responseSubmitAttachmentStateChanges :: SubmitAttachmentStateChangesResponse -> TestTree
responseSubmitAttachmentStateChanges =
  res
    "SubmitAttachmentStateChangesResponse"
    "fixture/SubmitAttachmentStateChangesResponse.proto"
    defaultService
    (Proxy :: Proxy SubmitAttachmentStateChanges)

responseRunTask :: RunTaskResponse -> TestTree
responseRunTask =
  res
    "RunTaskResponse"
    "fixture/RunTaskResponse.proto"
    defaultService
    (Proxy :: Proxy RunTask)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusters)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks =
  res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListTasks)

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListServices)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateService)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes =
  res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAttributes)

responsePutAccountSetting :: PutAccountSettingResponse -> TestTree
responsePutAccountSetting =
  res
    "PutAccountSettingResponse"
    "fixture/PutAccountSettingResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountSetting)

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

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDescribeContainerInstances :: DescribeContainerInstancesResponse -> TestTree
responseDescribeContainerInstances =
  res
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContainerInstances)

responseListAttributes :: ListAttributesResponse -> TestTree
responseListAttributes =
  res
    "ListAttributesResponse"
    "fixture/ListAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttributes)

responsePutAccountSettingDefault :: PutAccountSettingDefaultResponse -> TestTree
responsePutAccountSettingDefault =
  res
    "PutAccountSettingDefaultResponse"
    "fixture/PutAccountSettingDefaultResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccountSettingDefault)

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

responseDeleteCapacityProvider :: DeleteCapacityProviderResponse -> TestTree
responseDeleteCapacityProvider =
  res
    "DeleteCapacityProviderResponse"
    "fixture/DeleteCapacityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCapacityProvider)

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

responseUpdateCapacityProvider :: UpdateCapacityProviderResponse -> TestTree
responseUpdateCapacityProvider =
  res
    "UpdateCapacityProviderResponse"
    "fixture/UpdateCapacityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCapacityProvider)

responseCreateCapacityProvider :: CreateCapacityProviderResponse -> TestTree
responseCreateCapacityProvider =
  res
    "CreateCapacityProviderResponse"
    "fixture/CreateCapacityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCapacityProvider)

responseDescribeTaskSets :: DescribeTaskSetsResponse -> TestTree
responseDescribeTaskSets =
  res
    "DescribeTaskSetsResponse"
    "fixture/DescribeTaskSetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTaskSets)

responseRegisterTaskDefinition :: RegisterTaskDefinitionResponse -> TestTree
responseRegisterTaskDefinition =
  res
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTaskDefinition)

responseListTaskDefinitions :: ListTaskDefinitionsResponse -> TestTree
responseListTaskDefinitions =
  res
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTaskDefinitions)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes =
  res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutAttributes)

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

responseDeregisterContainerInstance :: DeregisterContainerInstanceResponse -> TestTree
responseDeregisterContainerInstance =
  res
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterContainerInstance)

responseUpdateTaskSet :: UpdateTaskSetResponse -> TestTree
responseUpdateTaskSet =
  res
    "UpdateTaskSetResponse"
    "fixture/UpdateTaskSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTaskSet)

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

responseUpdateContainerInstancesState :: UpdateContainerInstancesStateResponse -> TestTree
responseUpdateContainerInstancesState =
  res
    "UpdateContainerInstancesStateResponse"
    "fixture/UpdateContainerInstancesStateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContainerInstancesState)

responseDescribeCapacityProviders :: DescribeCapacityProvidersResponse -> TestTree
responseDescribeCapacityProviders =
  res
    "DescribeCapacityProvidersResponse"
    "fixture/DescribeCapacityProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCapacityProviders)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy :: Proxy ListClusters)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCluster)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices =
  res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServices)

responseSubmitTaskStateChange :: SubmitTaskStateChangeResponse -> TestTree
responseSubmitTaskStateChange =
  res
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse.proto"
    defaultService
    (Proxy :: Proxy SubmitTaskStateChange)

responseExecuteCommand :: ExecuteCommandResponse -> TestTree
responseExecuteCommand =
  res
    "ExecuteCommandResponse"
    "fixture/ExecuteCommandResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteCommand)

responseDeregisterTaskDefinition :: DeregisterTaskDefinitionResponse -> TestTree
responseDeregisterTaskDefinition =
  res
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTaskDefinition)

responseStartTask :: StartTaskResponse -> TestTree
responseStartTask =
  res
    "StartTaskResponse"
    "fixture/StartTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StartTask)

responseDescribeTaskDefinition :: DescribeTaskDefinitionResponse -> TestTree
responseDescribeTaskDefinition =
  res
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTaskDefinition)

responsePutClusterCapacityProviders :: PutClusterCapacityProvidersResponse -> TestTree
responsePutClusterCapacityProviders =
  res
    "PutClusterCapacityProvidersResponse"
    "fixture/PutClusterCapacityProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy PutClusterCapacityProviders)

responseStopTask :: StopTaskResponse -> TestTree
responseStopTask =
  res
    "StopTaskResponse"
    "fixture/StopTaskResponse.proto"
    defaultService
    (Proxy :: Proxy StopTask)

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
