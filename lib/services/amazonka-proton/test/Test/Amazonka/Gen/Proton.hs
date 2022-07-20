{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Proton
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         , requestCancelEnvironmentDeployment $
--             newCancelEnvironmentDeployment
--
--         , requestCancelServiceInstanceDeployment $
--             newCancelServiceInstanceDeployment
--
--         , requestCancelServicePipelineDeployment $
--             newCancelServicePipelineDeployment
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
--         , requestCreateService $
--             newCreateService
--
--         , requestCreateServiceTemplate $
--             newCreateServiceTemplate
--
--         , requestCreateServiceTemplateVersion $
--             newCreateServiceTemplateVersion
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
--         , requestDeleteService $
--             newDeleteService
--
--         , requestDeleteServiceTemplate $
--             newDeleteServiceTemplate
--
--         , requestDeleteServiceTemplateVersion $
--             newDeleteServiceTemplateVersion
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
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
--         , requestListEnvironmentAccountConnections $
--             newListEnvironmentAccountConnections
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
--         , requestListServiceInstances $
--             newListServiceInstances
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
--           ]

--     , testGroup "response"
--         [ responseAcceptEnvironmentAccountConnection $
--             newAcceptEnvironmentAccountConnectionResponse
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
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseCreateServiceTemplate $
--             newCreateServiceTemplateResponse
--
--         , responseCreateServiceTemplateVersion $
--             newCreateServiceTemplateVersionResponse
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
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseDeleteServiceTemplate $
--             newDeleteServiceTemplateResponse
--
--         , responseDeleteServiceTemplateVersion $
--             newDeleteServiceTemplateVersionResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
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
--         , responseListEnvironmentAccountConnections $
--             newListEnvironmentAccountConnectionsResponse
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
--         , responseListServiceInstances $
--             newListServiceInstancesResponse
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
--           ]
--     ]

-- Requests

requestAcceptEnvironmentAccountConnection :: AcceptEnvironmentAccountConnection -> TestTree
requestAcceptEnvironmentAccountConnection =
  req
    "AcceptEnvironmentAccountConnection"
    "fixture/AcceptEnvironmentAccountConnection.yaml"

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

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

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

requestListEnvironmentAccountConnections :: ListEnvironmentAccountConnections -> TestTree
requestListEnvironmentAccountConnections =
  req
    "ListEnvironmentAccountConnections"
    "fixture/ListEnvironmentAccountConnections.yaml"

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

requestListServiceInstances :: ListServiceInstances -> TestTree
requestListServiceInstances =
  req
    "ListServiceInstances"
    "fixture/ListServiceInstances.yaml"

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

-- Responses

responseAcceptEnvironmentAccountConnection :: AcceptEnvironmentAccountConnectionResponse -> TestTree
responseAcceptEnvironmentAccountConnection =
  res
    "AcceptEnvironmentAccountConnectionResponse"
    "fixture/AcceptEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptEnvironmentAccountConnection)

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

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSettings)

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

responseListEnvironmentAccountConnections :: ListEnvironmentAccountConnectionsResponse -> TestTree
responseListEnvironmentAccountConnections =
  res
    "ListEnvironmentAccountConnectionsResponse"
    "fixture/ListEnvironmentAccountConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentAccountConnections)

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

responseListServiceInstances :: ListServiceInstancesResponse -> TestTree
responseListServiceInstances =
  res
    "ListServiceInstancesResponse"
    "fixture/ListServiceInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceInstances)

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
