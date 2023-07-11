{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Proton
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Proton where

import Amazonka.Proton
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Proton.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptEnvironmentAccountConnection $
--             newAcceptEnvironmentAccountConnection
--
--         , requestCancelComponentDeployment $
--             newCancelComponentDeployment
--
--         , requestCancelEnvironmentDeployment $
--             newCancelEnvironmentDeployment
--
--         , requestCancelServiceInstanceDeployment $
--             newCancelServiceInstanceDeployment
--
--         , requestCancelServicePipelineDeployment $
--             newCancelServicePipelineDeployment
--
--         , requestCreateComponent $
--             newCreateComponent
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreateEnvironmentAccountConnection $
--             newCreateEnvironmentAccountConnection
--
--         , requestCreateEnvironmentTemplate $
--             newCreateEnvironmentTemplate
--
--         , requestCreateEnvironmentTemplateVersion $
--             newCreateEnvironmentTemplateVersion
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestCreateService $
--             newCreateService
--
--         , requestCreateServiceTemplate $
--             newCreateServiceTemplate
--
--         , requestCreateServiceTemplateVersion $
--             newCreateServiceTemplateVersion
--
--         , requestCreateTemplateSyncConfig $
--             newCreateTemplateSyncConfig
--
--         , requestDeleteComponent $
--             newDeleteComponent
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestDeleteEnvironmentAccountConnection $
--             newDeleteEnvironmentAccountConnection
--
--         , requestDeleteEnvironmentTemplate $
--             newDeleteEnvironmentTemplate
--
--         , requestDeleteEnvironmentTemplateVersion $
--             newDeleteEnvironmentTemplateVersion
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestDeleteServiceTemplate $
--             newDeleteServiceTemplate
--
--         , requestDeleteServiceTemplateVersion $
--             newDeleteServiceTemplateVersion
--
--         , requestDeleteTemplateSyncConfig $
--             newDeleteTemplateSyncConfig
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestGetComponent $
--             newGetComponent
--
--         , requestGetEnvironment $
--             newGetEnvironment
--
--         , requestGetEnvironmentAccountConnection $
--             newGetEnvironmentAccountConnection
--
--         , requestGetEnvironmentTemplate $
--             newGetEnvironmentTemplate
--
--         , requestGetEnvironmentTemplateVersion $
--             newGetEnvironmentTemplateVersion
--
--         , requestGetRepository $
--             newGetRepository
--
--         , requestGetRepositorySyncStatus $
--             newGetRepositorySyncStatus
--
--         , requestGetService $
--             newGetService
--
--         , requestGetServiceInstance $
--             newGetServiceInstance
--
--         , requestGetServiceTemplate $
--             newGetServiceTemplate
--
--         , requestGetServiceTemplateVersion $
--             newGetServiceTemplateVersion
--
--         , requestGetTemplateSyncConfig $
--             newGetTemplateSyncConfig
--
--         , requestGetTemplateSyncStatus $
--             newGetTemplateSyncStatus
--
--         , requestListComponentOutputs $
--             newListComponentOutputs
--
--         , requestListComponentProvisionedResources $
--             newListComponentProvisionedResources
--
--         , requestListComponents $
--             newListComponents
--
--         , requestListEnvironmentAccountConnections $
--             newListEnvironmentAccountConnections
--
--         , requestListEnvironmentOutputs $
--             newListEnvironmentOutputs
--
--         , requestListEnvironmentProvisionedResources $
--             newListEnvironmentProvisionedResources
--
--         , requestListEnvironmentTemplateVersions $
--             newListEnvironmentTemplateVersions
--
--         , requestListEnvironmentTemplates $
--             newListEnvironmentTemplates
--
--         , requestListEnvironments $
--             newListEnvironments
--
--         , requestListRepositories $
--             newListRepositories
--
--         , requestListRepositorySyncDefinitions $
--             newListRepositorySyncDefinitions
--
--         , requestListServiceInstanceOutputs $
--             newListServiceInstanceOutputs
--
--         , requestListServiceInstanceProvisionedResources $
--             newListServiceInstanceProvisionedResources
--
--         , requestListServiceInstances $
--             newListServiceInstances
--
--         , requestListServicePipelineOutputs $
--             newListServicePipelineOutputs
--
--         , requestListServicePipelineProvisionedResources $
--             newListServicePipelineProvisionedResources
--
--         , requestListServiceTemplateVersions $
--             newListServiceTemplateVersions
--
--         , requestListServiceTemplates $
--             newListServiceTemplates
--
--         , requestListServices $
--             newListServices
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestNotifyResourceDeploymentStatusChange $
--             newNotifyResourceDeploymentStatusChange
--
--         , requestRejectEnvironmentAccountConnection $
--             newRejectEnvironmentAccountConnection
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccountSettings $
--             newUpdateAccountSettings
--
--         , requestUpdateComponent $
--             newUpdateComponent
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestUpdateEnvironmentAccountConnection $
--             newUpdateEnvironmentAccountConnection
--
--         , requestUpdateEnvironmentTemplate $
--             newUpdateEnvironmentTemplate
--
--         , requestUpdateEnvironmentTemplateVersion $
--             newUpdateEnvironmentTemplateVersion
--
--         , requestUpdateService $
--             newUpdateService
--
--         , requestUpdateServiceInstance $
--             newUpdateServiceInstance
--
--         , requestUpdateServicePipeline $
--             newUpdateServicePipeline
--
--         , requestUpdateServiceTemplate $
--             newUpdateServiceTemplate
--
--         , requestUpdateServiceTemplateVersion $
--             newUpdateServiceTemplateVersion
--
--         , requestUpdateTemplateSyncConfig $
--             newUpdateTemplateSyncConfig
--
--           ]

--     , testGroup "response"
--         [ responseAcceptEnvironmentAccountConnection $
--             newAcceptEnvironmentAccountConnectionResponse
--
--         , responseCancelComponentDeployment $
--             newCancelComponentDeploymentResponse
--
--         , responseCancelEnvironmentDeployment $
--             newCancelEnvironmentDeploymentResponse
--
--         , responseCancelServiceInstanceDeployment $
--             newCancelServiceInstanceDeploymentResponse
--
--         , responseCancelServicePipelineDeployment $
--             newCancelServicePipelineDeploymentResponse
--
--         , responseCreateComponent $
--             newCreateComponentResponse
--
--         , responseCreateEnvironment $
--             newCreateEnvironmentResponse
--
--         , responseCreateEnvironmentAccountConnection $
--             newCreateEnvironmentAccountConnectionResponse
--
--         , responseCreateEnvironmentTemplate $
--             newCreateEnvironmentTemplateResponse
--
--         , responseCreateEnvironmentTemplateVersion $
--             newCreateEnvironmentTemplateVersionResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseCreateServiceTemplate $
--             newCreateServiceTemplateResponse
--
--         , responseCreateServiceTemplateVersion $
--             newCreateServiceTemplateVersionResponse
--
--         , responseCreateTemplateSyncConfig $
--             newCreateTemplateSyncConfigResponse
--
--         , responseDeleteComponent $
--             newDeleteComponentResponse
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseDeleteEnvironmentAccountConnection $
--             newDeleteEnvironmentAccountConnectionResponse
--
--         , responseDeleteEnvironmentTemplate $
--             newDeleteEnvironmentTemplateResponse
--
--         , responseDeleteEnvironmentTemplateVersion $
--             newDeleteEnvironmentTemplateVersionResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseDeleteServiceTemplate $
--             newDeleteServiceTemplateResponse
--
--         , responseDeleteServiceTemplateVersion $
--             newDeleteServiceTemplateVersionResponse
--
--         , responseDeleteTemplateSyncConfig $
--             newDeleteTemplateSyncConfigResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseGetComponent $
--             newGetComponentResponse
--
--         , responseGetEnvironment $
--             newGetEnvironmentResponse
--
--         , responseGetEnvironmentAccountConnection $
--             newGetEnvironmentAccountConnectionResponse
--
--         , responseGetEnvironmentTemplate $
--             newGetEnvironmentTemplateResponse
--
--         , responseGetEnvironmentTemplateVersion $
--             newGetEnvironmentTemplateVersionResponse
--
--         , responseGetRepository $
--             newGetRepositoryResponse
--
--         , responseGetRepositorySyncStatus $
--             newGetRepositorySyncStatusResponse
--
--         , responseGetService $
--             newGetServiceResponse
--
--         , responseGetServiceInstance $
--             newGetServiceInstanceResponse
--
--         , responseGetServiceTemplate $
--             newGetServiceTemplateResponse
--
--         , responseGetServiceTemplateVersion $
--             newGetServiceTemplateVersionResponse
--
--         , responseGetTemplateSyncConfig $
--             newGetTemplateSyncConfigResponse
--
--         , responseGetTemplateSyncStatus $
--             newGetTemplateSyncStatusResponse
--
--         , responseListComponentOutputs $
--             newListComponentOutputsResponse
--
--         , responseListComponentProvisionedResources $
--             newListComponentProvisionedResourcesResponse
--
--         , responseListComponents $
--             newListComponentsResponse
--
--         , responseListEnvironmentAccountConnections $
--             newListEnvironmentAccountConnectionsResponse
--
--         , responseListEnvironmentOutputs $
--             newListEnvironmentOutputsResponse
--
--         , responseListEnvironmentProvisionedResources $
--             newListEnvironmentProvisionedResourcesResponse
--
--         , responseListEnvironmentTemplateVersions $
--             newListEnvironmentTemplateVersionsResponse
--
--         , responseListEnvironmentTemplates $
--             newListEnvironmentTemplatesResponse
--
--         , responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseListRepositories $
--             newListRepositoriesResponse
--
--         , responseListRepositorySyncDefinitions $
--             newListRepositorySyncDefinitionsResponse
--
--         , responseListServiceInstanceOutputs $
--             newListServiceInstanceOutputsResponse
--
--         , responseListServiceInstanceProvisionedResources $
--             newListServiceInstanceProvisionedResourcesResponse
--
--         , responseListServiceInstances $
--             newListServiceInstancesResponse
--
--         , responseListServicePipelineOutputs $
--             newListServicePipelineOutputsResponse
--
--         , responseListServicePipelineProvisionedResources $
--             newListServicePipelineProvisionedResourcesResponse
--
--         , responseListServiceTemplateVersions $
--             newListServiceTemplateVersionsResponse
--
--         , responseListServiceTemplates $
--             newListServiceTemplatesResponse
--
--         , responseListServices $
--             newListServicesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseNotifyResourceDeploymentStatusChange $
--             newNotifyResourceDeploymentStatusChangeResponse
--
--         , responseRejectEnvironmentAccountConnection $
--             newRejectEnvironmentAccountConnectionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccountSettings $
--             newUpdateAccountSettingsResponse
--
--         , responseUpdateComponent $
--             newUpdateComponentResponse
--
--         , responseUpdateEnvironment $
--             newUpdateEnvironmentResponse
--
--         , responseUpdateEnvironmentAccountConnection $
--             newUpdateEnvironmentAccountConnectionResponse
--
--         , responseUpdateEnvironmentTemplate $
--             newUpdateEnvironmentTemplateResponse
--
--         , responseUpdateEnvironmentTemplateVersion $
--             newUpdateEnvironmentTemplateVersionResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--         , responseUpdateServiceInstance $
--             newUpdateServiceInstanceResponse
--
--         , responseUpdateServicePipeline $
--             newUpdateServicePipelineResponse
--
--         , responseUpdateServiceTemplate $
--             newUpdateServiceTemplateResponse
--
--         , responseUpdateServiceTemplateVersion $
--             newUpdateServiceTemplateVersionResponse
--
--         , responseUpdateTemplateSyncConfig $
--             newUpdateTemplateSyncConfigResponse
--
--           ]
--     ]

-- Requests

requestAcceptEnvironmentAccountConnection :: AcceptEnvironmentAccountConnection -> TestTree
requestAcceptEnvironmentAccountConnection =
  req
    "AcceptEnvironmentAccountConnection"
    "fixture/AcceptEnvironmentAccountConnection.yaml"

requestCancelComponentDeployment :: CancelComponentDeployment -> TestTree
requestCancelComponentDeployment =
  req
    "CancelComponentDeployment"
    "fixture/CancelComponentDeployment.yaml"

requestCancelEnvironmentDeployment :: CancelEnvironmentDeployment -> TestTree
requestCancelEnvironmentDeployment =
  req
    "CancelEnvironmentDeployment"
    "fixture/CancelEnvironmentDeployment.yaml"

requestCancelServiceInstanceDeployment :: CancelServiceInstanceDeployment -> TestTree
requestCancelServiceInstanceDeployment =
  req
    "CancelServiceInstanceDeployment"
    "fixture/CancelServiceInstanceDeployment.yaml"

requestCancelServicePipelineDeployment :: CancelServicePipelineDeployment -> TestTree
requestCancelServicePipelineDeployment =
  req
    "CancelServicePipelineDeployment"
    "fixture/CancelServicePipelineDeployment.yaml"

requestCreateComponent :: CreateComponent -> TestTree
requestCreateComponent =
  req
    "CreateComponent"
    "fixture/CreateComponent.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

requestCreateEnvironmentAccountConnection :: CreateEnvironmentAccountConnection -> TestTree
requestCreateEnvironmentAccountConnection =
  req
    "CreateEnvironmentAccountConnection"
    "fixture/CreateEnvironmentAccountConnection.yaml"

requestCreateEnvironmentTemplate :: CreateEnvironmentTemplate -> TestTree
requestCreateEnvironmentTemplate =
  req
    "CreateEnvironmentTemplate"
    "fixture/CreateEnvironmentTemplate.yaml"

requestCreateEnvironmentTemplateVersion :: CreateEnvironmentTemplateVersion -> TestTree
requestCreateEnvironmentTemplateVersion =
  req
    "CreateEnvironmentTemplateVersion"
    "fixture/CreateEnvironmentTemplateVersion.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository =
  req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

requestCreateServiceTemplate :: CreateServiceTemplate -> TestTree
requestCreateServiceTemplate =
  req
    "CreateServiceTemplate"
    "fixture/CreateServiceTemplate.yaml"

requestCreateServiceTemplateVersion :: CreateServiceTemplateVersion -> TestTree
requestCreateServiceTemplateVersion =
  req
    "CreateServiceTemplateVersion"
    "fixture/CreateServiceTemplateVersion.yaml"

requestCreateTemplateSyncConfig :: CreateTemplateSyncConfig -> TestTree
requestCreateTemplateSyncConfig =
  req
    "CreateTemplateSyncConfig"
    "fixture/CreateTemplateSyncConfig.yaml"

requestDeleteComponent :: DeleteComponent -> TestTree
requestDeleteComponent =
  req
    "DeleteComponent"
    "fixture/DeleteComponent.yaml"

requestDeleteEnvironment :: DeleteEnvironment -> TestTree
requestDeleteEnvironment =
  req
    "DeleteEnvironment"
    "fixture/DeleteEnvironment.yaml"

requestDeleteEnvironmentAccountConnection :: DeleteEnvironmentAccountConnection -> TestTree
requestDeleteEnvironmentAccountConnection =
  req
    "DeleteEnvironmentAccountConnection"
    "fixture/DeleteEnvironmentAccountConnection.yaml"

requestDeleteEnvironmentTemplate :: DeleteEnvironmentTemplate -> TestTree
requestDeleteEnvironmentTemplate =
  req
    "DeleteEnvironmentTemplate"
    "fixture/DeleteEnvironmentTemplate.yaml"

requestDeleteEnvironmentTemplateVersion :: DeleteEnvironmentTemplateVersion -> TestTree
requestDeleteEnvironmentTemplateVersion =
  req
    "DeleteEnvironmentTemplateVersion"
    "fixture/DeleteEnvironmentTemplateVersion.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository =
  req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestDeleteService :: DeleteService -> TestTree
requestDeleteService =
  req
    "DeleteService"
    "fixture/DeleteService.yaml"

requestDeleteServiceTemplate :: DeleteServiceTemplate -> TestTree
requestDeleteServiceTemplate =
  req
    "DeleteServiceTemplate"
    "fixture/DeleteServiceTemplate.yaml"

requestDeleteServiceTemplateVersion :: DeleteServiceTemplateVersion -> TestTree
requestDeleteServiceTemplateVersion =
  req
    "DeleteServiceTemplateVersion"
    "fixture/DeleteServiceTemplateVersion.yaml"

requestDeleteTemplateSyncConfig :: DeleteTemplateSyncConfig -> TestTree
requestDeleteTemplateSyncConfig =
  req
    "DeleteTemplateSyncConfig"
    "fixture/DeleteTemplateSyncConfig.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestGetComponent :: GetComponent -> TestTree
requestGetComponent =
  req
    "GetComponent"
    "fixture/GetComponent.yaml"

requestGetEnvironment :: GetEnvironment -> TestTree
requestGetEnvironment =
  req
    "GetEnvironment"
    "fixture/GetEnvironment.yaml"

requestGetEnvironmentAccountConnection :: GetEnvironmentAccountConnection -> TestTree
requestGetEnvironmentAccountConnection =
  req
    "GetEnvironmentAccountConnection"
    "fixture/GetEnvironmentAccountConnection.yaml"

requestGetEnvironmentTemplate :: GetEnvironmentTemplate -> TestTree
requestGetEnvironmentTemplate =
  req
    "GetEnvironmentTemplate"
    "fixture/GetEnvironmentTemplate.yaml"

requestGetEnvironmentTemplateVersion :: GetEnvironmentTemplateVersion -> TestTree
requestGetEnvironmentTemplateVersion =
  req
    "GetEnvironmentTemplateVersion"
    "fixture/GetEnvironmentTemplateVersion.yaml"

requestGetRepository :: GetRepository -> TestTree
requestGetRepository =
  req
    "GetRepository"
    "fixture/GetRepository.yaml"

requestGetRepositorySyncStatus :: GetRepositorySyncStatus -> TestTree
requestGetRepositorySyncStatus =
  req
    "GetRepositorySyncStatus"
    "fixture/GetRepositorySyncStatus.yaml"

requestGetService :: GetService -> TestTree
requestGetService =
  req
    "GetService"
    "fixture/GetService.yaml"

requestGetServiceInstance :: GetServiceInstance -> TestTree
requestGetServiceInstance =
  req
    "GetServiceInstance"
    "fixture/GetServiceInstance.yaml"

requestGetServiceTemplate :: GetServiceTemplate -> TestTree
requestGetServiceTemplate =
  req
    "GetServiceTemplate"
    "fixture/GetServiceTemplate.yaml"

requestGetServiceTemplateVersion :: GetServiceTemplateVersion -> TestTree
requestGetServiceTemplateVersion =
  req
    "GetServiceTemplateVersion"
    "fixture/GetServiceTemplateVersion.yaml"

requestGetTemplateSyncConfig :: GetTemplateSyncConfig -> TestTree
requestGetTemplateSyncConfig =
  req
    "GetTemplateSyncConfig"
    "fixture/GetTemplateSyncConfig.yaml"

requestGetTemplateSyncStatus :: GetTemplateSyncStatus -> TestTree
requestGetTemplateSyncStatus =
  req
    "GetTemplateSyncStatus"
    "fixture/GetTemplateSyncStatus.yaml"

requestListComponentOutputs :: ListComponentOutputs -> TestTree
requestListComponentOutputs =
  req
    "ListComponentOutputs"
    "fixture/ListComponentOutputs.yaml"

requestListComponentProvisionedResources :: ListComponentProvisionedResources -> TestTree
requestListComponentProvisionedResources =
  req
    "ListComponentProvisionedResources"
    "fixture/ListComponentProvisionedResources.yaml"

requestListComponents :: ListComponents -> TestTree
requestListComponents =
  req
    "ListComponents"
    "fixture/ListComponents.yaml"

requestListEnvironmentAccountConnections :: ListEnvironmentAccountConnections -> TestTree
requestListEnvironmentAccountConnections =
  req
    "ListEnvironmentAccountConnections"
    "fixture/ListEnvironmentAccountConnections.yaml"

requestListEnvironmentOutputs :: ListEnvironmentOutputs -> TestTree
requestListEnvironmentOutputs =
  req
    "ListEnvironmentOutputs"
    "fixture/ListEnvironmentOutputs.yaml"

requestListEnvironmentProvisionedResources :: ListEnvironmentProvisionedResources -> TestTree
requestListEnvironmentProvisionedResources =
  req
    "ListEnvironmentProvisionedResources"
    "fixture/ListEnvironmentProvisionedResources.yaml"

requestListEnvironmentTemplateVersions :: ListEnvironmentTemplateVersions -> TestTree
requestListEnvironmentTemplateVersions =
  req
    "ListEnvironmentTemplateVersions"
    "fixture/ListEnvironmentTemplateVersions.yaml"

requestListEnvironmentTemplates :: ListEnvironmentTemplates -> TestTree
requestListEnvironmentTemplates =
  req
    "ListEnvironmentTemplates"
    "fixture/ListEnvironmentTemplates.yaml"

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestListRepositories :: ListRepositories -> TestTree
requestListRepositories =
  req
    "ListRepositories"
    "fixture/ListRepositories.yaml"

requestListRepositorySyncDefinitions :: ListRepositorySyncDefinitions -> TestTree
requestListRepositorySyncDefinitions =
  req
    "ListRepositorySyncDefinitions"
    "fixture/ListRepositorySyncDefinitions.yaml"

requestListServiceInstanceOutputs :: ListServiceInstanceOutputs -> TestTree
requestListServiceInstanceOutputs =
  req
    "ListServiceInstanceOutputs"
    "fixture/ListServiceInstanceOutputs.yaml"

requestListServiceInstanceProvisionedResources :: ListServiceInstanceProvisionedResources -> TestTree
requestListServiceInstanceProvisionedResources =
  req
    "ListServiceInstanceProvisionedResources"
    "fixture/ListServiceInstanceProvisionedResources.yaml"

requestListServiceInstances :: ListServiceInstances -> TestTree
requestListServiceInstances =
  req
    "ListServiceInstances"
    "fixture/ListServiceInstances.yaml"

requestListServicePipelineOutputs :: ListServicePipelineOutputs -> TestTree
requestListServicePipelineOutputs =
  req
    "ListServicePipelineOutputs"
    "fixture/ListServicePipelineOutputs.yaml"

requestListServicePipelineProvisionedResources :: ListServicePipelineProvisionedResources -> TestTree
requestListServicePipelineProvisionedResources =
  req
    "ListServicePipelineProvisionedResources"
    "fixture/ListServicePipelineProvisionedResources.yaml"

requestListServiceTemplateVersions :: ListServiceTemplateVersions -> TestTree
requestListServiceTemplateVersions =
  req
    "ListServiceTemplateVersions"
    "fixture/ListServiceTemplateVersions.yaml"

requestListServiceTemplates :: ListServiceTemplates -> TestTree
requestListServiceTemplates =
  req
    "ListServiceTemplates"
    "fixture/ListServiceTemplates.yaml"

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestNotifyResourceDeploymentStatusChange :: NotifyResourceDeploymentStatusChange -> TestTree
requestNotifyResourceDeploymentStatusChange =
  req
    "NotifyResourceDeploymentStatusChange"
    "fixture/NotifyResourceDeploymentStatusChange.yaml"

requestRejectEnvironmentAccountConnection :: RejectEnvironmentAccountConnection -> TestTree
requestRejectEnvironmentAccountConnection =
  req
    "RejectEnvironmentAccountConnection"
    "fixture/RejectEnvironmentAccountConnection.yaml"

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

requestUpdateAccountSettings :: UpdateAccountSettings -> TestTree
requestUpdateAccountSettings =
  req
    "UpdateAccountSettings"
    "fixture/UpdateAccountSettings.yaml"

requestUpdateComponent :: UpdateComponent -> TestTree
requestUpdateComponent =
  req
    "UpdateComponent"
    "fixture/UpdateComponent.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestUpdateEnvironmentAccountConnection :: UpdateEnvironmentAccountConnection -> TestTree
requestUpdateEnvironmentAccountConnection =
  req
    "UpdateEnvironmentAccountConnection"
    "fixture/UpdateEnvironmentAccountConnection.yaml"

requestUpdateEnvironmentTemplate :: UpdateEnvironmentTemplate -> TestTree
requestUpdateEnvironmentTemplate =
  req
    "UpdateEnvironmentTemplate"
    "fixture/UpdateEnvironmentTemplate.yaml"

requestUpdateEnvironmentTemplateVersion :: UpdateEnvironmentTemplateVersion -> TestTree
requestUpdateEnvironmentTemplateVersion =
  req
    "UpdateEnvironmentTemplateVersion"
    "fixture/UpdateEnvironmentTemplateVersion.yaml"

requestUpdateService :: UpdateService -> TestTree
requestUpdateService =
  req
    "UpdateService"
    "fixture/UpdateService.yaml"

requestUpdateServiceInstance :: UpdateServiceInstance -> TestTree
requestUpdateServiceInstance =
  req
    "UpdateServiceInstance"
    "fixture/UpdateServiceInstance.yaml"

requestUpdateServicePipeline :: UpdateServicePipeline -> TestTree
requestUpdateServicePipeline =
  req
    "UpdateServicePipeline"
    "fixture/UpdateServicePipeline.yaml"

requestUpdateServiceTemplate :: UpdateServiceTemplate -> TestTree
requestUpdateServiceTemplate =
  req
    "UpdateServiceTemplate"
    "fixture/UpdateServiceTemplate.yaml"

requestUpdateServiceTemplateVersion :: UpdateServiceTemplateVersion -> TestTree
requestUpdateServiceTemplateVersion =
  req
    "UpdateServiceTemplateVersion"
    "fixture/UpdateServiceTemplateVersion.yaml"

requestUpdateTemplateSyncConfig :: UpdateTemplateSyncConfig -> TestTree
requestUpdateTemplateSyncConfig =
  req
    "UpdateTemplateSyncConfig"
    "fixture/UpdateTemplateSyncConfig.yaml"

-- Responses

responseAcceptEnvironmentAccountConnection :: AcceptEnvironmentAccountConnectionResponse -> TestTree
responseAcceptEnvironmentAccountConnection =
  res
    "AcceptEnvironmentAccountConnectionResponse"
    "fixture/AcceptEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptEnvironmentAccountConnection)

responseCancelComponentDeployment :: CancelComponentDeploymentResponse -> TestTree
responseCancelComponentDeployment =
  res
    "CancelComponentDeploymentResponse"
    "fixture/CancelComponentDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelComponentDeployment)

responseCancelEnvironmentDeployment :: CancelEnvironmentDeploymentResponse -> TestTree
responseCancelEnvironmentDeployment =
  res
    "CancelEnvironmentDeploymentResponse"
    "fixture/CancelEnvironmentDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelEnvironmentDeployment)

responseCancelServiceInstanceDeployment :: CancelServiceInstanceDeploymentResponse -> TestTree
responseCancelServiceInstanceDeployment =
  res
    "CancelServiceInstanceDeploymentResponse"
    "fixture/CancelServiceInstanceDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelServiceInstanceDeployment)

responseCancelServicePipelineDeployment :: CancelServicePipelineDeploymentResponse -> TestTree
responseCancelServicePipelineDeployment =
  res
    "CancelServicePipelineDeploymentResponse"
    "fixture/CancelServicePipelineDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelServicePipelineDeployment)

responseCreateComponent :: CreateComponentResponse -> TestTree
responseCreateComponent =
  res
    "CreateComponentResponse"
    "fixture/CreateComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComponent)

responseCreateEnvironment :: CreateEnvironmentResponse -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironment)

responseCreateEnvironmentAccountConnection :: CreateEnvironmentAccountConnectionResponse -> TestTree
responseCreateEnvironmentAccountConnection =
  res
    "CreateEnvironmentAccountConnectionResponse"
    "fixture/CreateEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironmentAccountConnection)

responseCreateEnvironmentTemplate :: CreateEnvironmentTemplateResponse -> TestTree
responseCreateEnvironmentTemplate =
  res
    "CreateEnvironmentTemplateResponse"
    "fixture/CreateEnvironmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironmentTemplate)

responseCreateEnvironmentTemplateVersion :: CreateEnvironmentTemplateVersionResponse -> TestTree
responseCreateEnvironmentTemplateVersion =
  res
    "CreateEnvironmentTemplateVersionResponse"
    "fixture/CreateEnvironmentTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironmentTemplateVersion)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRepository)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)

responseCreateServiceTemplate :: CreateServiceTemplateResponse -> TestTree
responseCreateServiceTemplate =
  res
    "CreateServiceTemplateResponse"
    "fixture/CreateServiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceTemplate)

responseCreateServiceTemplateVersion :: CreateServiceTemplateVersionResponse -> TestTree
responseCreateServiceTemplateVersion =
  res
    "CreateServiceTemplateVersionResponse"
    "fixture/CreateServiceTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceTemplateVersion)

responseCreateTemplateSyncConfig :: CreateTemplateSyncConfigResponse -> TestTree
responseCreateTemplateSyncConfig =
  res
    "CreateTemplateSyncConfigResponse"
    "fixture/CreateTemplateSyncConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTemplateSyncConfig)

responseDeleteComponent :: DeleteComponentResponse -> TestTree
responseDeleteComponent =
  res
    "DeleteComponentResponse"
    "fixture/DeleteComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteComponent)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironment)

responseDeleteEnvironmentAccountConnection :: DeleteEnvironmentAccountConnectionResponse -> TestTree
responseDeleteEnvironmentAccountConnection =
  res
    "DeleteEnvironmentAccountConnectionResponse"
    "fixture/DeleteEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironmentAccountConnection)

responseDeleteEnvironmentTemplate :: DeleteEnvironmentTemplateResponse -> TestTree
responseDeleteEnvironmentTemplate =
  res
    "DeleteEnvironmentTemplateResponse"
    "fixture/DeleteEnvironmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironmentTemplate)

responseDeleteEnvironmentTemplateVersion :: DeleteEnvironmentTemplateVersionResponse -> TestTree
responseDeleteEnvironmentTemplateVersion =
  res
    "DeleteEnvironmentTemplateVersionResponse"
    "fixture/DeleteEnvironmentTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironmentTemplateVersion)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepository)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteService)

responseDeleteServiceTemplate :: DeleteServiceTemplateResponse -> TestTree
responseDeleteServiceTemplate =
  res
    "DeleteServiceTemplateResponse"
    "fixture/DeleteServiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceTemplate)

responseDeleteServiceTemplateVersion :: DeleteServiceTemplateVersionResponse -> TestTree
responseDeleteServiceTemplateVersion =
  res
    "DeleteServiceTemplateVersionResponse"
    "fixture/DeleteServiceTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceTemplateVersion)

responseDeleteTemplateSyncConfig :: DeleteTemplateSyncConfigResponse -> TestTree
responseDeleteTemplateSyncConfig =
  res
    "DeleteTemplateSyncConfigResponse"
    "fixture/DeleteTemplateSyncConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTemplateSyncConfig)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSettings)

responseGetComponent :: GetComponentResponse -> TestTree
responseGetComponent =
  res
    "GetComponentResponse"
    "fixture/GetComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComponent)

responseGetEnvironment :: GetEnvironmentResponse -> TestTree
responseGetEnvironment =
  res
    "GetEnvironmentResponse"
    "fixture/GetEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironment)

responseGetEnvironmentAccountConnection :: GetEnvironmentAccountConnectionResponse -> TestTree
responseGetEnvironmentAccountConnection =
  res
    "GetEnvironmentAccountConnectionResponse"
    "fixture/GetEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironmentAccountConnection)

responseGetEnvironmentTemplate :: GetEnvironmentTemplateResponse -> TestTree
responseGetEnvironmentTemplate =
  res
    "GetEnvironmentTemplateResponse"
    "fixture/GetEnvironmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironmentTemplate)

responseGetEnvironmentTemplateVersion :: GetEnvironmentTemplateVersionResponse -> TestTree
responseGetEnvironmentTemplateVersion =
  res
    "GetEnvironmentTemplateVersionResponse"
    "fixture/GetEnvironmentTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironmentTemplateVersion)

responseGetRepository :: GetRepositoryResponse -> TestTree
responseGetRepository =
  res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepository)

responseGetRepositorySyncStatus :: GetRepositorySyncStatusResponse -> TestTree
responseGetRepositorySyncStatus =
  res
    "GetRepositorySyncStatusResponse"
    "fixture/GetRepositorySyncStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositorySyncStatus)

responseGetService :: GetServiceResponse -> TestTree
responseGetService =
  res
    "GetServiceResponse"
    "fixture/GetServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetService)

responseGetServiceInstance :: GetServiceInstanceResponse -> TestTree
responseGetServiceInstance =
  res
    "GetServiceInstanceResponse"
    "fixture/GetServiceInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceInstance)

responseGetServiceTemplate :: GetServiceTemplateResponse -> TestTree
responseGetServiceTemplate =
  res
    "GetServiceTemplateResponse"
    "fixture/GetServiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceTemplate)

responseGetServiceTemplateVersion :: GetServiceTemplateVersionResponse -> TestTree
responseGetServiceTemplateVersion =
  res
    "GetServiceTemplateVersionResponse"
    "fixture/GetServiceTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceTemplateVersion)

responseGetTemplateSyncConfig :: GetTemplateSyncConfigResponse -> TestTree
responseGetTemplateSyncConfig =
  res
    "GetTemplateSyncConfigResponse"
    "fixture/GetTemplateSyncConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplateSyncConfig)

responseGetTemplateSyncStatus :: GetTemplateSyncStatusResponse -> TestTree
responseGetTemplateSyncStatus =
  res
    "GetTemplateSyncStatusResponse"
    "fixture/GetTemplateSyncStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTemplateSyncStatus)

responseListComponentOutputs :: ListComponentOutputsResponse -> TestTree
responseListComponentOutputs =
  res
    "ListComponentOutputsResponse"
    "fixture/ListComponentOutputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponentOutputs)

responseListComponentProvisionedResources :: ListComponentProvisionedResourcesResponse -> TestTree
responseListComponentProvisionedResources =
  res
    "ListComponentProvisionedResourcesResponse"
    "fixture/ListComponentProvisionedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponentProvisionedResources)

responseListComponents :: ListComponentsResponse -> TestTree
responseListComponents =
  res
    "ListComponentsResponse"
    "fixture/ListComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponents)

responseListEnvironmentAccountConnections :: ListEnvironmentAccountConnectionsResponse -> TestTree
responseListEnvironmentAccountConnections =
  res
    "ListEnvironmentAccountConnectionsResponse"
    "fixture/ListEnvironmentAccountConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentAccountConnections)

responseListEnvironmentOutputs :: ListEnvironmentOutputsResponse -> TestTree
responseListEnvironmentOutputs =
  res
    "ListEnvironmentOutputsResponse"
    "fixture/ListEnvironmentOutputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentOutputs)

responseListEnvironmentProvisionedResources :: ListEnvironmentProvisionedResourcesResponse -> TestTree
responseListEnvironmentProvisionedResources =
  res
    "ListEnvironmentProvisionedResourcesResponse"
    "fixture/ListEnvironmentProvisionedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentProvisionedResources)

responseListEnvironmentTemplateVersions :: ListEnvironmentTemplateVersionsResponse -> TestTree
responseListEnvironmentTemplateVersions =
  res
    "ListEnvironmentTemplateVersionsResponse"
    "fixture/ListEnvironmentTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentTemplateVersions)

responseListEnvironmentTemplates :: ListEnvironmentTemplatesResponse -> TestTree
responseListEnvironmentTemplates =
  res
    "ListEnvironmentTemplatesResponse"
    "fixture/ListEnvironmentTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentTemplates)

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironments)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories =
  res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositories)

responseListRepositorySyncDefinitions :: ListRepositorySyncDefinitionsResponse -> TestTree
responseListRepositorySyncDefinitions =
  res
    "ListRepositorySyncDefinitionsResponse"
    "fixture/ListRepositorySyncDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositorySyncDefinitions)

responseListServiceInstanceOutputs :: ListServiceInstanceOutputsResponse -> TestTree
responseListServiceInstanceOutputs =
  res
    "ListServiceInstanceOutputsResponse"
    "fixture/ListServiceInstanceOutputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceInstanceOutputs)

responseListServiceInstanceProvisionedResources :: ListServiceInstanceProvisionedResourcesResponse -> TestTree
responseListServiceInstanceProvisionedResources =
  res
    "ListServiceInstanceProvisionedResourcesResponse"
    "fixture/ListServiceInstanceProvisionedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceInstanceProvisionedResources)

responseListServiceInstances :: ListServiceInstancesResponse -> TestTree
responseListServiceInstances =
  res
    "ListServiceInstancesResponse"
    "fixture/ListServiceInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceInstances)

responseListServicePipelineOutputs :: ListServicePipelineOutputsResponse -> TestTree
responseListServicePipelineOutputs =
  res
    "ListServicePipelineOutputsResponse"
    "fixture/ListServicePipelineOutputsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServicePipelineOutputs)

responseListServicePipelineProvisionedResources :: ListServicePipelineProvisionedResourcesResponse -> TestTree
responseListServicePipelineProvisionedResources =
  res
    "ListServicePipelineProvisionedResourcesResponse"
    "fixture/ListServicePipelineProvisionedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServicePipelineProvisionedResources)

responseListServiceTemplateVersions :: ListServiceTemplateVersionsResponse -> TestTree
responseListServiceTemplateVersions =
  res
    "ListServiceTemplateVersionsResponse"
    "fixture/ListServiceTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceTemplateVersions)

responseListServiceTemplates :: ListServiceTemplatesResponse -> TestTree
responseListServiceTemplates =
  res
    "ListServiceTemplatesResponse"
    "fixture/ListServiceTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceTemplates)

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServices)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseNotifyResourceDeploymentStatusChange :: NotifyResourceDeploymentStatusChangeResponse -> TestTree
responseNotifyResourceDeploymentStatusChange =
  res
    "NotifyResourceDeploymentStatusChangeResponse"
    "fixture/NotifyResourceDeploymentStatusChangeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyResourceDeploymentStatusChange)

responseRejectEnvironmentAccountConnection :: RejectEnvironmentAccountConnectionResponse -> TestTree
responseRejectEnvironmentAccountConnection =
  res
    "RejectEnvironmentAccountConnectionResponse"
    "fixture/RejectEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectEnvironmentAccountConnection)

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

responseUpdateAccountSettings :: UpdateAccountSettingsResponse -> TestTree
responseUpdateAccountSettings =
  res
    "UpdateAccountSettingsResponse"
    "fixture/UpdateAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountSettings)

responseUpdateComponent :: UpdateComponentResponse -> TestTree
responseUpdateComponent =
  res
    "UpdateComponentResponse"
    "fixture/UpdateComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateComponent)

responseUpdateEnvironment :: UpdateEnvironmentResponse -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironment)

responseUpdateEnvironmentAccountConnection :: UpdateEnvironmentAccountConnectionResponse -> TestTree
responseUpdateEnvironmentAccountConnection =
  res
    "UpdateEnvironmentAccountConnectionResponse"
    "fixture/UpdateEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironmentAccountConnection)

responseUpdateEnvironmentTemplate :: UpdateEnvironmentTemplateResponse -> TestTree
responseUpdateEnvironmentTemplate =
  res
    "UpdateEnvironmentTemplateResponse"
    "fixture/UpdateEnvironmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironmentTemplate)

responseUpdateEnvironmentTemplateVersion :: UpdateEnvironmentTemplateVersionResponse -> TestTree
responseUpdateEnvironmentTemplateVersion =
  res
    "UpdateEnvironmentTemplateVersionResponse"
    "fixture/UpdateEnvironmentTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironmentTemplateVersion)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateService)

responseUpdateServiceInstance :: UpdateServiceInstanceResponse -> TestTree
responseUpdateServiceInstance =
  res
    "UpdateServiceInstanceResponse"
    "fixture/UpdateServiceInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceInstance)

responseUpdateServicePipeline :: UpdateServicePipelineResponse -> TestTree
responseUpdateServicePipeline =
  res
    "UpdateServicePipelineResponse"
    "fixture/UpdateServicePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServicePipeline)

responseUpdateServiceTemplate :: UpdateServiceTemplateResponse -> TestTree
responseUpdateServiceTemplate =
  res
    "UpdateServiceTemplateResponse"
    "fixture/UpdateServiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceTemplate)

responseUpdateServiceTemplateVersion :: UpdateServiceTemplateVersionResponse -> TestTree
responseUpdateServiceTemplateVersion =
  res
    "UpdateServiceTemplateVersionResponse"
    "fixture/UpdateServiceTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceTemplateVersion)

responseUpdateTemplateSyncConfig :: UpdateTemplateSyncConfigResponse -> TestTree
responseUpdateTemplateSyncConfig =
  res
    "UpdateTemplateSyncConfigResponse"
    "fixture/UpdateTemplateSyncConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTemplateSyncConfig)
