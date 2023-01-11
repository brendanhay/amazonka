{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ECS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ECS where

import Amazonka.ECS
import qualified Data.Proxy as Proxy
import Test.Amazonka.ECS.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateCapacityProvider $
--             newCreateCapacityProvider
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateService $
--             newCreateService
--
--         , requestCreateTaskSet $
--             newCreateTaskSet
--
--         , requestDeleteAccountSetting $
--             newDeleteAccountSetting
--
--         , requestDeleteAttributes $
--             newDeleteAttributes
--
--         , requestDeleteCapacityProvider $
--             newDeleteCapacityProvider
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestDeleteTaskSet $
--             newDeleteTaskSet
--
--         , requestDeregisterContainerInstance $
--             newDeregisterContainerInstance
--
--         , requestDeregisterTaskDefinition $
--             newDeregisterTaskDefinition
--
--         , requestDescribeCapacityProviders $
--             newDescribeCapacityProviders
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestDescribeContainerInstances $
--             newDescribeContainerInstances
--
--         , requestDescribeServices $
--             newDescribeServices
--
--         , requestDescribeTaskDefinition $
--             newDescribeTaskDefinition
--
--         , requestDescribeTaskSets $
--             newDescribeTaskSets
--
--         , requestDescribeTasks $
--             newDescribeTasks
--
--         , requestDiscoverPollEndpoint $
--             newDiscoverPollEndpoint
--
--         , requestExecuteCommand $
--             newExecuteCommand
--
--         , requestGetTaskProtection $
--             newGetTaskProtection
--
--         , requestListAccountSettings $
--             newListAccountSettings
--
--         , requestListAttributes $
--             newListAttributes
--
--         , requestListClusters $
--             newListClusters
--
--         , requestListContainerInstances $
--             newListContainerInstances
--
--         , requestListServices $
--             newListServices
--
--         , requestListServicesByNamespace $
--             newListServicesByNamespace
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTaskDefinitionFamilies $
--             newListTaskDefinitionFamilies
--
--         , requestListTaskDefinitions $
--             newListTaskDefinitions
--
--         , requestListTasks $
--             newListTasks
--
--         , requestPutAccountSetting $
--             newPutAccountSetting
--
--         , requestPutAccountSettingDefault $
--             newPutAccountSettingDefault
--
--         , requestPutAttributes $
--             newPutAttributes
--
--         , requestPutClusterCapacityProviders $
--             newPutClusterCapacityProviders
--
--         , requestRegisterContainerInstance $
--             newRegisterContainerInstance
--
--         , requestRegisterTaskDefinition $
--             newRegisterTaskDefinition
--
--         , requestRunTask $
--             newRunTask
--
--         , requestStartTask $
--             newStartTask
--
--         , requestStopTask $
--             newStopTask
--
--         , requestSubmitAttachmentStateChanges $
--             newSubmitAttachmentStateChanges
--
--         , requestSubmitContainerStateChange $
--             newSubmitContainerStateChange
--
--         , requestSubmitTaskStateChange $
--             newSubmitTaskStateChange
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCapacityProvider $
--             newUpdateCapacityProvider
--
--         , requestUpdateCluster $
--             newUpdateCluster
--
--         , requestUpdateClusterSettings $
--             newUpdateClusterSettings
--
--         , requestUpdateContainerAgent $
--             newUpdateContainerAgent
--
--         , requestUpdateContainerInstancesState $
--             newUpdateContainerInstancesState
--
--         , requestUpdateService $
--             newUpdateService
--
--         , requestUpdateServicePrimaryTaskSet $
--             newUpdateServicePrimaryTaskSet
--
--         , requestUpdateTaskProtection $
--             newUpdateTaskProtection
--
--         , requestUpdateTaskSet $
--             newUpdateTaskSet
--
--           ]

--     , testGroup "response"
--         [ responseCreateCapacityProvider $
--             newCreateCapacityProviderResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseCreateTaskSet $
--             newCreateTaskSetResponse
--
--         , responseDeleteAccountSetting $
--             newDeleteAccountSettingResponse
--
--         , responseDeleteAttributes $
--             newDeleteAttributesResponse
--
--         , responseDeleteCapacityProvider $
--             newDeleteCapacityProviderResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseDeleteTaskSet $
--             newDeleteTaskSetResponse
--
--         , responseDeregisterContainerInstance $
--             newDeregisterContainerInstanceResponse
--
--         , responseDeregisterTaskDefinition $
--             newDeregisterTaskDefinitionResponse
--
--         , responseDescribeCapacityProviders $
--             newDescribeCapacityProvidersResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseDescribeContainerInstances $
--             newDescribeContainerInstancesResponse
--
--         , responseDescribeServices $
--             newDescribeServicesResponse
--
--         , responseDescribeTaskDefinition $
--             newDescribeTaskDefinitionResponse
--
--         , responseDescribeTaskSets $
--             newDescribeTaskSetsResponse
--
--         , responseDescribeTasks $
--             newDescribeTasksResponse
--
--         , responseDiscoverPollEndpoint $
--             newDiscoverPollEndpointResponse
--
--         , responseExecuteCommand $
--             newExecuteCommandResponse
--
--         , responseGetTaskProtection $
--             newGetTaskProtectionResponse
--
--         , responseListAccountSettings $
--             newListAccountSettingsResponse
--
--         , responseListAttributes $
--             newListAttributesResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseListContainerInstances $
--             newListContainerInstancesResponse
--
--         , responseListServices $
--             newListServicesResponse
--
--         , responseListServicesByNamespace $
--             newListServicesByNamespaceResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTaskDefinitionFamilies $
--             newListTaskDefinitionFamiliesResponse
--
--         , responseListTaskDefinitions $
--             newListTaskDefinitionsResponse
--
--         , responseListTasks $
--             newListTasksResponse
--
--         , responsePutAccountSetting $
--             newPutAccountSettingResponse
--
--         , responsePutAccountSettingDefault $
--             newPutAccountSettingDefaultResponse
--
--         , responsePutAttributes $
--             newPutAttributesResponse
--
--         , responsePutClusterCapacityProviders $
--             newPutClusterCapacityProvidersResponse
--
--         , responseRegisterContainerInstance $
--             newRegisterContainerInstanceResponse
--
--         , responseRegisterTaskDefinition $
--             newRegisterTaskDefinitionResponse
--
--         , responseRunTask $
--             newRunTaskResponse
--
--         , responseStartTask $
--             newStartTaskResponse
--
--         , responseStopTask $
--             newStopTaskResponse
--
--         , responseSubmitAttachmentStateChanges $
--             newSubmitAttachmentStateChangesResponse
--
--         , responseSubmitContainerStateChange $
--             newSubmitContainerStateChangeResponse
--
--         , responseSubmitTaskStateChange $
--             newSubmitTaskStateChangeResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCapacityProvider $
--             newUpdateCapacityProviderResponse
--
--         , responseUpdateCluster $
--             newUpdateClusterResponse
--
--         , responseUpdateClusterSettings $
--             newUpdateClusterSettingsResponse
--
--         , responseUpdateContainerAgent $
--             newUpdateContainerAgentResponse
--
--         , responseUpdateContainerInstancesState $
--             newUpdateContainerInstancesStateResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--         , responseUpdateServicePrimaryTaskSet $
--             newUpdateServicePrimaryTaskSetResponse
--
--         , responseUpdateTaskProtection $
--             newUpdateTaskProtectionResponse
--
--         , responseUpdateTaskSet $
--             newUpdateTaskSetResponse
--
--           ]
--     ]

-- Requests

requestCreateCapacityProvider :: CreateCapacityProvider -> TestTree
requestCreateCapacityProvider =
  req
    "CreateCapacityProvider"
    "fixture/CreateCapacityProvider.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

requestCreateTaskSet :: CreateTaskSet -> TestTree
requestCreateTaskSet =
  req
    "CreateTaskSet"
    "fixture/CreateTaskSet.yaml"

requestDeleteAccountSetting :: DeleteAccountSetting -> TestTree
requestDeleteAccountSetting =
  req
    "DeleteAccountSetting"
    "fixture/DeleteAccountSetting.yaml"

requestDeleteAttributes :: DeleteAttributes -> TestTree
requestDeleteAttributes =
  req
    "DeleteAttributes"
    "fixture/DeleteAttributes.yaml"

requestDeleteCapacityProvider :: DeleteCapacityProvider -> TestTree
requestDeleteCapacityProvider =
  req
    "DeleteCapacityProvider"
    "fixture/DeleteCapacityProvider.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDeleteService :: DeleteService -> TestTree
requestDeleteService =
  req
    "DeleteService"
    "fixture/DeleteService.yaml"

requestDeleteTaskSet :: DeleteTaskSet -> TestTree
requestDeleteTaskSet =
  req
    "DeleteTaskSet"
    "fixture/DeleteTaskSet.yaml"

requestDeregisterContainerInstance :: DeregisterContainerInstance -> TestTree
requestDeregisterContainerInstance =
  req
    "DeregisterContainerInstance"
    "fixture/DeregisterContainerInstance.yaml"

requestDeregisterTaskDefinition :: DeregisterTaskDefinition -> TestTree
requestDeregisterTaskDefinition =
  req
    "DeregisterTaskDefinition"
    "fixture/DeregisterTaskDefinition.yaml"

requestDescribeCapacityProviders :: DescribeCapacityProviders -> TestTree
requestDescribeCapacityProviders =
  req
    "DescribeCapacityProviders"
    "fixture/DescribeCapacityProviders.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDescribeContainerInstances :: DescribeContainerInstances -> TestTree
requestDescribeContainerInstances =
  req
    "DescribeContainerInstances"
    "fixture/DescribeContainerInstances.yaml"

requestDescribeServices :: DescribeServices -> TestTree
requestDescribeServices =
  req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

requestDescribeTaskDefinition :: DescribeTaskDefinition -> TestTree
requestDescribeTaskDefinition =
  req
    "DescribeTaskDefinition"
    "fixture/DescribeTaskDefinition.yaml"

requestDescribeTaskSets :: DescribeTaskSets -> TestTree
requestDescribeTaskSets =
  req
    "DescribeTaskSets"
    "fixture/DescribeTaskSets.yaml"

requestDescribeTasks :: DescribeTasks -> TestTree
requestDescribeTasks =
  req
    "DescribeTasks"
    "fixture/DescribeTasks.yaml"

requestDiscoverPollEndpoint :: DiscoverPollEndpoint -> TestTree
requestDiscoverPollEndpoint =
  req
    "DiscoverPollEndpoint"
    "fixture/DiscoverPollEndpoint.yaml"

requestExecuteCommand :: ExecuteCommand -> TestTree
requestExecuteCommand =
  req
    "ExecuteCommand"
    "fixture/ExecuteCommand.yaml"

requestGetTaskProtection :: GetTaskProtection -> TestTree
requestGetTaskProtection =
  req
    "GetTaskProtection"
    "fixture/GetTaskProtection.yaml"

requestListAccountSettings :: ListAccountSettings -> TestTree
requestListAccountSettings =
  req
    "ListAccountSettings"
    "fixture/ListAccountSettings.yaml"

requestListAttributes :: ListAttributes -> TestTree
requestListAttributes =
  req
    "ListAttributes"
    "fixture/ListAttributes.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestListContainerInstances :: ListContainerInstances -> TestTree
requestListContainerInstances =
  req
    "ListContainerInstances"
    "fixture/ListContainerInstances.yaml"

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestListServicesByNamespace :: ListServicesByNamespace -> TestTree
requestListServicesByNamespace =
  req
    "ListServicesByNamespace"
    "fixture/ListServicesByNamespace.yaml"

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

requestListTaskDefinitions :: ListTaskDefinitions -> TestTree
requestListTaskDefinitions =
  req
    "ListTaskDefinitions"
    "fixture/ListTaskDefinitions.yaml"

requestListTasks :: ListTasks -> TestTree
requestListTasks =
  req
    "ListTasks"
    "fixture/ListTasks.yaml"

requestPutAccountSetting :: PutAccountSetting -> TestTree
requestPutAccountSetting =
  req
    "PutAccountSetting"
    "fixture/PutAccountSetting.yaml"

requestPutAccountSettingDefault :: PutAccountSettingDefault -> TestTree
requestPutAccountSettingDefault =
  req
    "PutAccountSettingDefault"
    "fixture/PutAccountSettingDefault.yaml"

requestPutAttributes :: PutAttributes -> TestTree
requestPutAttributes =
  req
    "PutAttributes"
    "fixture/PutAttributes.yaml"

requestPutClusterCapacityProviders :: PutClusterCapacityProviders -> TestTree
requestPutClusterCapacityProviders =
  req
    "PutClusterCapacityProviders"
    "fixture/PutClusterCapacityProviders.yaml"

requestRegisterContainerInstance :: RegisterContainerInstance -> TestTree
requestRegisterContainerInstance =
  req
    "RegisterContainerInstance"
    "fixture/RegisterContainerInstance.yaml"

requestRegisterTaskDefinition :: RegisterTaskDefinition -> TestTree
requestRegisterTaskDefinition =
  req
    "RegisterTaskDefinition"
    "fixture/RegisterTaskDefinition.yaml"

requestRunTask :: RunTask -> TestTree
requestRunTask =
  req
    "RunTask"
    "fixture/RunTask.yaml"

requestStartTask :: StartTask -> TestTree
requestStartTask =
  req
    "StartTask"
    "fixture/StartTask.yaml"

requestStopTask :: StopTask -> TestTree
requestStopTask =
  req
    "StopTask"
    "fixture/StopTask.yaml"

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

requestSubmitTaskStateChange :: SubmitTaskStateChange -> TestTree
requestSubmitTaskStateChange =
  req
    "SubmitTaskStateChange"
    "fixture/SubmitTaskStateChange.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateCapacityProvider :: UpdateCapacityProvider -> TestTree
requestUpdateCapacityProvider =
  req
    "UpdateCapacityProvider"
    "fixture/UpdateCapacityProvider.yaml"

requestUpdateCluster :: UpdateCluster -> TestTree
requestUpdateCluster =
  req
    "UpdateCluster"
    "fixture/UpdateCluster.yaml"

requestUpdateClusterSettings :: UpdateClusterSettings -> TestTree
requestUpdateClusterSettings =
  req
    "UpdateClusterSettings"
    "fixture/UpdateClusterSettings.yaml"

requestUpdateContainerAgent :: UpdateContainerAgent -> TestTree
requestUpdateContainerAgent =
  req
    "UpdateContainerAgent"
    "fixture/UpdateContainerAgent.yaml"

requestUpdateContainerInstancesState :: UpdateContainerInstancesState -> TestTree
requestUpdateContainerInstancesState =
  req
    "UpdateContainerInstancesState"
    "fixture/UpdateContainerInstancesState.yaml"

requestUpdateService :: UpdateService -> TestTree
requestUpdateService =
  req
    "UpdateService"
    "fixture/UpdateService.yaml"

requestUpdateServicePrimaryTaskSet :: UpdateServicePrimaryTaskSet -> TestTree
requestUpdateServicePrimaryTaskSet =
  req
    "UpdateServicePrimaryTaskSet"
    "fixture/UpdateServicePrimaryTaskSet.yaml"

requestUpdateTaskProtection :: UpdateTaskProtection -> TestTree
requestUpdateTaskProtection =
  req
    "UpdateTaskProtection"
    "fixture/UpdateTaskProtection.yaml"

requestUpdateTaskSet :: UpdateTaskSet -> TestTree
requestUpdateTaskSet =
  req
    "UpdateTaskSet"
    "fixture/UpdateTaskSet.yaml"

-- Responses

responseCreateCapacityProvider :: CreateCapacityProviderResponse -> TestTree
responseCreateCapacityProvider =
  res
    "CreateCapacityProviderResponse"
    "fixture/CreateCapacityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCapacityProvider)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)

responseCreateTaskSet :: CreateTaskSetResponse -> TestTree
responseCreateTaskSet =
  res
    "CreateTaskSetResponse"
    "fixture/CreateTaskSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTaskSet)

responseDeleteAccountSetting :: DeleteAccountSettingResponse -> TestTree
responseDeleteAccountSetting =
  res
    "DeleteAccountSettingResponse"
    "fixture/DeleteAccountSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountSetting)

responseDeleteAttributes :: DeleteAttributesResponse -> TestTree
responseDeleteAttributes =
  res
    "DeleteAttributesResponse"
    "fixture/DeleteAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAttributes)

responseDeleteCapacityProvider :: DeleteCapacityProviderResponse -> TestTree
responseDeleteCapacityProvider =
  res
    "DeleteCapacityProviderResponse"
    "fixture/DeleteCapacityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCapacityProvider)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteService)

responseDeleteTaskSet :: DeleteTaskSetResponse -> TestTree
responseDeleteTaskSet =
  res
    "DeleteTaskSetResponse"
    "fixture/DeleteTaskSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTaskSet)

responseDeregisterContainerInstance :: DeregisterContainerInstanceResponse -> TestTree
responseDeregisterContainerInstance =
  res
    "DeregisterContainerInstanceResponse"
    "fixture/DeregisterContainerInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterContainerInstance)

responseDeregisterTaskDefinition :: DeregisterTaskDefinitionResponse -> TestTree
responseDeregisterTaskDefinition =
  res
    "DeregisterTaskDefinitionResponse"
    "fixture/DeregisterTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTaskDefinition)

responseDescribeCapacityProviders :: DescribeCapacityProvidersResponse -> TestTree
responseDescribeCapacityProviders =
  res
    "DescribeCapacityProvidersResponse"
    "fixture/DescribeCapacityProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCapacityProviders)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusters)

responseDescribeContainerInstances :: DescribeContainerInstancesResponse -> TestTree
responseDescribeContainerInstances =
  res
    "DescribeContainerInstancesResponse"
    "fixture/DescribeContainerInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContainerInstances)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices =
  res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServices)

responseDescribeTaskDefinition :: DescribeTaskDefinitionResponse -> TestTree
responseDescribeTaskDefinition =
  res
    "DescribeTaskDefinitionResponse"
    "fixture/DescribeTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTaskDefinition)

responseDescribeTaskSets :: DescribeTaskSetsResponse -> TestTree
responseDescribeTaskSets =
  res
    "DescribeTaskSetsResponse"
    "fixture/DescribeTaskSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTaskSets)

responseDescribeTasks :: DescribeTasksResponse -> TestTree
responseDescribeTasks =
  res
    "DescribeTasksResponse"
    "fixture/DescribeTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTasks)

responseDiscoverPollEndpoint :: DiscoverPollEndpointResponse -> TestTree
responseDiscoverPollEndpoint =
  res
    "DiscoverPollEndpointResponse"
    "fixture/DiscoverPollEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DiscoverPollEndpoint)

responseExecuteCommand :: ExecuteCommandResponse -> TestTree
responseExecuteCommand =
  res
    "ExecuteCommandResponse"
    "fixture/ExecuteCommandResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteCommand)

responseGetTaskProtection :: GetTaskProtectionResponse -> TestTree
responseGetTaskProtection =
  res
    "GetTaskProtectionResponse"
    "fixture/GetTaskProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTaskProtection)

responseListAccountSettings :: ListAccountSettingsResponse -> TestTree
responseListAccountSettings =
  res
    "ListAccountSettingsResponse"
    "fixture/ListAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountSettings)

responseListAttributes :: ListAttributesResponse -> TestTree
responseListAttributes =
  res
    "ListAttributesResponse"
    "fixture/ListAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttributes)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseListContainerInstances :: ListContainerInstancesResponse -> TestTree
responseListContainerInstances =
  res
    "ListContainerInstancesResponse"
    "fixture/ListContainerInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContainerInstances)

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServices)

responseListServicesByNamespace :: ListServicesByNamespaceResponse -> TestTree
responseListServicesByNamespace =
  res
    "ListServicesByNamespaceResponse"
    "fixture/ListServicesByNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServicesByNamespace)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTaskDefinitionFamilies :: ListTaskDefinitionFamiliesResponse -> TestTree
responseListTaskDefinitionFamilies =
  res
    "ListTaskDefinitionFamiliesResponse"
    "fixture/ListTaskDefinitionFamiliesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTaskDefinitionFamilies)

responseListTaskDefinitions :: ListTaskDefinitionsResponse -> TestTree
responseListTaskDefinitions =
  res
    "ListTaskDefinitionsResponse"
    "fixture/ListTaskDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTaskDefinitions)

responseListTasks :: ListTasksResponse -> TestTree
responseListTasks =
  res
    "ListTasksResponse"
    "fixture/ListTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTasks)

responsePutAccountSetting :: PutAccountSettingResponse -> TestTree
responsePutAccountSetting =
  res
    "PutAccountSettingResponse"
    "fixture/PutAccountSettingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountSetting)

responsePutAccountSettingDefault :: PutAccountSettingDefaultResponse -> TestTree
responsePutAccountSettingDefault =
  res
    "PutAccountSettingDefaultResponse"
    "fixture/PutAccountSettingDefaultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccountSettingDefault)

responsePutAttributes :: PutAttributesResponse -> TestTree
responsePutAttributes =
  res
    "PutAttributesResponse"
    "fixture/PutAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAttributes)

responsePutClusterCapacityProviders :: PutClusterCapacityProvidersResponse -> TestTree
responsePutClusterCapacityProviders =
  res
    "PutClusterCapacityProvidersResponse"
    "fixture/PutClusterCapacityProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutClusterCapacityProviders)

responseRegisterContainerInstance :: RegisterContainerInstanceResponse -> TestTree
responseRegisterContainerInstance =
  res
    "RegisterContainerInstanceResponse"
    "fixture/RegisterContainerInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterContainerInstance)

responseRegisterTaskDefinition :: RegisterTaskDefinitionResponse -> TestTree
responseRegisterTaskDefinition =
  res
    "RegisterTaskDefinitionResponse"
    "fixture/RegisterTaskDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTaskDefinition)

responseRunTask :: RunTaskResponse -> TestTree
responseRunTask =
  res
    "RunTaskResponse"
    "fixture/RunTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RunTask)

responseStartTask :: StartTaskResponse -> TestTree
responseStartTask =
  res
    "StartTaskResponse"
    "fixture/StartTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTask)

responseStopTask :: StopTaskResponse -> TestTree
responseStopTask =
  res
    "StopTaskResponse"
    "fixture/StopTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTask)

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

responseSubmitTaskStateChange :: SubmitTaskStateChangeResponse -> TestTree
responseSubmitTaskStateChange =
  res
    "SubmitTaskStateChangeResponse"
    "fixture/SubmitTaskStateChangeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubmitTaskStateChange)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateCapacityProvider :: UpdateCapacityProviderResponse -> TestTree
responseUpdateCapacityProvider =
  res
    "UpdateCapacityProviderResponse"
    "fixture/UpdateCapacityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCapacityProvider)

responseUpdateCluster :: UpdateClusterResponse -> TestTree
responseUpdateCluster =
  res
    "UpdateClusterResponse"
    "fixture/UpdateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCluster)

responseUpdateClusterSettings :: UpdateClusterSettingsResponse -> TestTree
responseUpdateClusterSettings =
  res
    "UpdateClusterSettingsResponse"
    "fixture/UpdateClusterSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClusterSettings)

responseUpdateContainerAgent :: UpdateContainerAgentResponse -> TestTree
responseUpdateContainerAgent =
  res
    "UpdateContainerAgentResponse"
    "fixture/UpdateContainerAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContainerAgent)

responseUpdateContainerInstancesState :: UpdateContainerInstancesStateResponse -> TestTree
responseUpdateContainerInstancesState =
  res
    "UpdateContainerInstancesStateResponse"
    "fixture/UpdateContainerInstancesStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContainerInstancesState)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateService)

responseUpdateServicePrimaryTaskSet :: UpdateServicePrimaryTaskSetResponse -> TestTree
responseUpdateServicePrimaryTaskSet =
  res
    "UpdateServicePrimaryTaskSetResponse"
    "fixture/UpdateServicePrimaryTaskSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServicePrimaryTaskSet)

responseUpdateTaskProtection :: UpdateTaskProtectionResponse -> TestTree
responseUpdateTaskProtection =
  res
    "UpdateTaskProtectionResponse"
    "fixture/UpdateTaskProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTaskProtection)

responseUpdateTaskSet :: UpdateTaskSetResponse -> TestTree
responseUpdateTaskSet =
  res
    "UpdateTaskSetResponse"
    "fixture/UpdateTaskSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTaskSet)
