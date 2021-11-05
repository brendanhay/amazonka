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
--         [ requestListServices $
--             newListServices
--
--         , requestListEnvironments $
--             newListEnvironments
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestUpdateService $
--             newUpdateService
--
--         , requestGetServiceInstance $
--             newGetServiceInstance
--
--         , requestAcceptEnvironmentAccountConnection $
--             newAcceptEnvironmentAccountConnection
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestUpdateAccountSettings $
--             newUpdateAccountSettings
--
--         , requestRejectEnvironmentAccountConnection $
--             newRejectEnvironmentAccountConnection
--
--         , requestListServiceInstances $
--             newListServiceInstances
--
--         , requestCancelServicePipelineDeployment $
--             newCancelServicePipelineDeployment
--
--         , requestCreateServiceTemplateVersion $
--             newCreateServiceTemplateVersion
--
--         , requestGetServiceTemplate $
--             newGetServiceTemplate
--
--         , requestCreateEnvironmentTemplateVersion $
--             newCreateEnvironmentTemplateVersion
--
--         , requestCancelServiceInstanceDeployment $
--             newCancelServiceInstanceDeployment
--
--         , requestGetEnvironmentTemplate $
--             newGetEnvironmentTemplate
--
--         , requestUpdateServicePipeline $
--             newUpdateServicePipeline
--
--         , requestListServiceTemplateVersions $
--             newListServiceTemplateVersions
--
--         , requestCreateEnvironmentAccountConnection $
--             newCreateEnvironmentAccountConnection
--
--         , requestListEnvironmentTemplateVersions $
--             newListEnvironmentTemplateVersions
--
--         , requestGetEnvironmentTemplateVersion $
--             newGetEnvironmentTemplateVersion
--
--         , requestCreateServiceTemplate $
--             newCreateServiceTemplate
--
--         , requestGetServiceTemplateVersion $
--             newGetServiceTemplateVersion
--
--         , requestCreateEnvironmentTemplate $
--             newCreateEnvironmentTemplate
--
--         , requestDeleteEnvironmentTemplate $
--             newDeleteEnvironmentTemplate
--
--         , requestUpdateEnvironmentTemplate $
--             newUpdateEnvironmentTemplate
--
--         , requestGetEnvironmentAccountConnection $
--             newGetEnvironmentAccountConnection
--
--         , requestListEnvironmentTemplates $
--             newListEnvironmentTemplates
--
--         , requestDeleteServiceTemplate $
--             newDeleteServiceTemplate
--
--         , requestUpdateServiceTemplate $
--             newUpdateServiceTemplate
--
--         , requestListServiceTemplates $
--             newListServiceTemplates
--
--         , requestGetAccountSettings $
--             newGetAccountSettings
--
--         , requestCancelEnvironmentDeployment $
--             newCancelEnvironmentDeployment
--
--         , requestUpdateServiceInstance $
--             newUpdateServiceInstance
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetEnvironment $
--             newGetEnvironment
--
--         , requestListEnvironmentAccountConnections $
--             newListEnvironmentAccountConnections
--
--         , requestGetService $
--             newGetService
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteEnvironmentAccountConnection $
--             newDeleteEnvironmentAccountConnection
--
--         , requestUpdateEnvironmentAccountConnection $
--             newUpdateEnvironmentAccountConnection
--
--         , requestDeleteServiceTemplateVersion $
--             newDeleteServiceTemplateVersion
--
--         , requestUpdateServiceTemplateVersion $
--             newUpdateServiceTemplateVersion
--
--         , requestUpdateEnvironmentTemplateVersion $
--             newUpdateEnvironmentTemplateVersion
--
--         , requestDeleteEnvironmentTemplateVersion $
--             newDeleteEnvironmentTemplateVersion
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreateService $
--             newCreateService
--
--           ]

--     , testGroup "response"
--         [ responseListServices $
--             newListServicesResponse
--
--         , responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseUpdateEnvironment $
--             newUpdateEnvironmentResponse
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--         , responseGetServiceInstance $
--             newGetServiceInstanceResponse
--
--         , responseAcceptEnvironmentAccountConnection $
--             newAcceptEnvironmentAccountConnectionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseUpdateAccountSettings $
--             newUpdateAccountSettingsResponse
--
--         , responseRejectEnvironmentAccountConnection $
--             newRejectEnvironmentAccountConnectionResponse
--
--         , responseListServiceInstances $
--             newListServiceInstancesResponse
--
--         , responseCancelServicePipelineDeployment $
--             newCancelServicePipelineDeploymentResponse
--
--         , responseCreateServiceTemplateVersion $
--             newCreateServiceTemplateVersionResponse
--
--         , responseGetServiceTemplate $
--             newGetServiceTemplateResponse
--
--         , responseCreateEnvironmentTemplateVersion $
--             newCreateEnvironmentTemplateVersionResponse
--
--         , responseCancelServiceInstanceDeployment $
--             newCancelServiceInstanceDeploymentResponse
--
--         , responseGetEnvironmentTemplate $
--             newGetEnvironmentTemplateResponse
--
--         , responseUpdateServicePipeline $
--             newUpdateServicePipelineResponse
--
--         , responseListServiceTemplateVersions $
--             newListServiceTemplateVersionsResponse
--
--         , responseCreateEnvironmentAccountConnection $
--             newCreateEnvironmentAccountConnectionResponse
--
--         , responseListEnvironmentTemplateVersions $
--             newListEnvironmentTemplateVersionsResponse
--
--         , responseGetEnvironmentTemplateVersion $
--             newGetEnvironmentTemplateVersionResponse
--
--         , responseCreateServiceTemplate $
--             newCreateServiceTemplateResponse
--
--         , responseGetServiceTemplateVersion $
--             newGetServiceTemplateVersionResponse
--
--         , responseCreateEnvironmentTemplate $
--             newCreateEnvironmentTemplateResponse
--
--         , responseDeleteEnvironmentTemplate $
--             newDeleteEnvironmentTemplateResponse
--
--         , responseUpdateEnvironmentTemplate $
--             newUpdateEnvironmentTemplateResponse
--
--         , responseGetEnvironmentAccountConnection $
--             newGetEnvironmentAccountConnectionResponse
--
--         , responseListEnvironmentTemplates $
--             newListEnvironmentTemplatesResponse
--
--         , responseDeleteServiceTemplate $
--             newDeleteServiceTemplateResponse
--
--         , responseUpdateServiceTemplate $
--             newUpdateServiceTemplateResponse
--
--         , responseListServiceTemplates $
--             newListServiceTemplatesResponse
--
--         , responseGetAccountSettings $
--             newGetAccountSettingsResponse
--
--         , responseCancelEnvironmentDeployment $
--             newCancelEnvironmentDeploymentResponse
--
--         , responseUpdateServiceInstance $
--             newUpdateServiceInstanceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetEnvironment $
--             newGetEnvironmentResponse
--
--         , responseListEnvironmentAccountConnections $
--             newListEnvironmentAccountConnectionsResponse
--
--         , responseGetService $
--             newGetServiceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteEnvironmentAccountConnection $
--             newDeleteEnvironmentAccountConnectionResponse
--
--         , responseUpdateEnvironmentAccountConnection $
--             newUpdateEnvironmentAccountConnectionResponse
--
--         , responseDeleteServiceTemplateVersion $
--             newDeleteServiceTemplateVersionResponse
--
--         , responseUpdateServiceTemplateVersion $
--             newUpdateServiceTemplateVersionResponse
--
--         , responseUpdateEnvironmentTemplateVersion $
--             newUpdateEnvironmentTemplateVersionResponse
--
--         , responseDeleteEnvironmentTemplateVersion $
--             newDeleteEnvironmentTemplateVersionResponse
--
--         , responseCreateEnvironment $
--             newCreateEnvironmentResponse
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

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestDeleteEnvironment :: DeleteEnvironment -> TestTree
requestDeleteEnvironment =
  req
    "DeleteEnvironment"
    "fixture/DeleteEnvironment.yaml"

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

requestGetServiceInstance :: GetServiceInstance -> TestTree
requestGetServiceInstance =
  req
    "GetServiceInstance"
    "fixture/GetServiceInstance.yaml"

requestAcceptEnvironmentAccountConnection :: AcceptEnvironmentAccountConnection -> TestTree
requestAcceptEnvironmentAccountConnection =
  req
    "AcceptEnvironmentAccountConnection"
    "fixture/AcceptEnvironmentAccountConnection.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestUpdateAccountSettings :: UpdateAccountSettings -> TestTree
requestUpdateAccountSettings =
  req
    "UpdateAccountSettings"
    "fixture/UpdateAccountSettings.yaml"

requestRejectEnvironmentAccountConnection :: RejectEnvironmentAccountConnection -> TestTree
requestRejectEnvironmentAccountConnection =
  req
    "RejectEnvironmentAccountConnection"
    "fixture/RejectEnvironmentAccountConnection.yaml"

requestListServiceInstances :: ListServiceInstances -> TestTree
requestListServiceInstances =
  req
    "ListServiceInstances"
    "fixture/ListServiceInstances.yaml"

requestCancelServicePipelineDeployment :: CancelServicePipelineDeployment -> TestTree
requestCancelServicePipelineDeployment =
  req
    "CancelServicePipelineDeployment"
    "fixture/CancelServicePipelineDeployment.yaml"

requestCreateServiceTemplateVersion :: CreateServiceTemplateVersion -> TestTree
requestCreateServiceTemplateVersion =
  req
    "CreateServiceTemplateVersion"
    "fixture/CreateServiceTemplateVersion.yaml"

requestGetServiceTemplate :: GetServiceTemplate -> TestTree
requestGetServiceTemplate =
  req
    "GetServiceTemplate"
    "fixture/GetServiceTemplate.yaml"

requestCreateEnvironmentTemplateVersion :: CreateEnvironmentTemplateVersion -> TestTree
requestCreateEnvironmentTemplateVersion =
  req
    "CreateEnvironmentTemplateVersion"
    "fixture/CreateEnvironmentTemplateVersion.yaml"

requestCancelServiceInstanceDeployment :: CancelServiceInstanceDeployment -> TestTree
requestCancelServiceInstanceDeployment =
  req
    "CancelServiceInstanceDeployment"
    "fixture/CancelServiceInstanceDeployment.yaml"

requestGetEnvironmentTemplate :: GetEnvironmentTemplate -> TestTree
requestGetEnvironmentTemplate =
  req
    "GetEnvironmentTemplate"
    "fixture/GetEnvironmentTemplate.yaml"

requestUpdateServicePipeline :: UpdateServicePipeline -> TestTree
requestUpdateServicePipeline =
  req
    "UpdateServicePipeline"
    "fixture/UpdateServicePipeline.yaml"

requestListServiceTemplateVersions :: ListServiceTemplateVersions -> TestTree
requestListServiceTemplateVersions =
  req
    "ListServiceTemplateVersions"
    "fixture/ListServiceTemplateVersions.yaml"

requestCreateEnvironmentAccountConnection :: CreateEnvironmentAccountConnection -> TestTree
requestCreateEnvironmentAccountConnection =
  req
    "CreateEnvironmentAccountConnection"
    "fixture/CreateEnvironmentAccountConnection.yaml"

requestListEnvironmentTemplateVersions :: ListEnvironmentTemplateVersions -> TestTree
requestListEnvironmentTemplateVersions =
  req
    "ListEnvironmentTemplateVersions"
    "fixture/ListEnvironmentTemplateVersions.yaml"

requestGetEnvironmentTemplateVersion :: GetEnvironmentTemplateVersion -> TestTree
requestGetEnvironmentTemplateVersion =
  req
    "GetEnvironmentTemplateVersion"
    "fixture/GetEnvironmentTemplateVersion.yaml"

requestCreateServiceTemplate :: CreateServiceTemplate -> TestTree
requestCreateServiceTemplate =
  req
    "CreateServiceTemplate"
    "fixture/CreateServiceTemplate.yaml"

requestGetServiceTemplateVersion :: GetServiceTemplateVersion -> TestTree
requestGetServiceTemplateVersion =
  req
    "GetServiceTemplateVersion"
    "fixture/GetServiceTemplateVersion.yaml"

requestCreateEnvironmentTemplate :: CreateEnvironmentTemplate -> TestTree
requestCreateEnvironmentTemplate =
  req
    "CreateEnvironmentTemplate"
    "fixture/CreateEnvironmentTemplate.yaml"

requestDeleteEnvironmentTemplate :: DeleteEnvironmentTemplate -> TestTree
requestDeleteEnvironmentTemplate =
  req
    "DeleteEnvironmentTemplate"
    "fixture/DeleteEnvironmentTemplate.yaml"

requestUpdateEnvironmentTemplate :: UpdateEnvironmentTemplate -> TestTree
requestUpdateEnvironmentTemplate =
  req
    "UpdateEnvironmentTemplate"
    "fixture/UpdateEnvironmentTemplate.yaml"

requestGetEnvironmentAccountConnection :: GetEnvironmentAccountConnection -> TestTree
requestGetEnvironmentAccountConnection =
  req
    "GetEnvironmentAccountConnection"
    "fixture/GetEnvironmentAccountConnection.yaml"

requestListEnvironmentTemplates :: ListEnvironmentTemplates -> TestTree
requestListEnvironmentTemplates =
  req
    "ListEnvironmentTemplates"
    "fixture/ListEnvironmentTemplates.yaml"

requestDeleteServiceTemplate :: DeleteServiceTemplate -> TestTree
requestDeleteServiceTemplate =
  req
    "DeleteServiceTemplate"
    "fixture/DeleteServiceTemplate.yaml"

requestUpdateServiceTemplate :: UpdateServiceTemplate -> TestTree
requestUpdateServiceTemplate =
  req
    "UpdateServiceTemplate"
    "fixture/UpdateServiceTemplate.yaml"

requestListServiceTemplates :: ListServiceTemplates -> TestTree
requestListServiceTemplates =
  req
    "ListServiceTemplates"
    "fixture/ListServiceTemplates.yaml"

requestGetAccountSettings :: GetAccountSettings -> TestTree
requestGetAccountSettings =
  req
    "GetAccountSettings"
    "fixture/GetAccountSettings.yaml"

requestCancelEnvironmentDeployment :: CancelEnvironmentDeployment -> TestTree
requestCancelEnvironmentDeployment =
  req
    "CancelEnvironmentDeployment"
    "fixture/CancelEnvironmentDeployment.yaml"

requestUpdateServiceInstance :: UpdateServiceInstance -> TestTree
requestUpdateServiceInstance =
  req
    "UpdateServiceInstance"
    "fixture/UpdateServiceInstance.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetEnvironment :: GetEnvironment -> TestTree
requestGetEnvironment =
  req
    "GetEnvironment"
    "fixture/GetEnvironment.yaml"

requestListEnvironmentAccountConnections :: ListEnvironmentAccountConnections -> TestTree
requestListEnvironmentAccountConnections =
  req
    "ListEnvironmentAccountConnections"
    "fixture/ListEnvironmentAccountConnections.yaml"

requestGetService :: GetService -> TestTree
requestGetService =
  req
    "GetService"
    "fixture/GetService.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteEnvironmentAccountConnection :: DeleteEnvironmentAccountConnection -> TestTree
requestDeleteEnvironmentAccountConnection =
  req
    "DeleteEnvironmentAccountConnection"
    "fixture/DeleteEnvironmentAccountConnection.yaml"

requestUpdateEnvironmentAccountConnection :: UpdateEnvironmentAccountConnection -> TestTree
requestUpdateEnvironmentAccountConnection =
  req
    "UpdateEnvironmentAccountConnection"
    "fixture/UpdateEnvironmentAccountConnection.yaml"

requestDeleteServiceTemplateVersion :: DeleteServiceTemplateVersion -> TestTree
requestDeleteServiceTemplateVersion =
  req
    "DeleteServiceTemplateVersion"
    "fixture/DeleteServiceTemplateVersion.yaml"

requestUpdateServiceTemplateVersion :: UpdateServiceTemplateVersion -> TestTree
requestUpdateServiceTemplateVersion =
  req
    "UpdateServiceTemplateVersion"
    "fixture/UpdateServiceTemplateVersion.yaml"

requestUpdateEnvironmentTemplateVersion :: UpdateEnvironmentTemplateVersion -> TestTree
requestUpdateEnvironmentTemplateVersion =
  req
    "UpdateEnvironmentTemplateVersion"
    "fixture/UpdateEnvironmentTemplateVersion.yaml"

requestDeleteEnvironmentTemplateVersion :: DeleteEnvironmentTemplateVersion -> TestTree
requestDeleteEnvironmentTemplateVersion =
  req
    "DeleteEnvironmentTemplateVersion"
    "fixture/DeleteEnvironmentTemplateVersion.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

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

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironments)

responseUpdateEnvironment :: UpdateEnvironmentResponse -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironment)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironment)

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

responseGetServiceInstance :: GetServiceInstanceResponse -> TestTree
responseGetServiceInstance =
  res
    "GetServiceInstanceResponse"
    "fixture/GetServiceInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceInstance)

responseAcceptEnvironmentAccountConnection :: AcceptEnvironmentAccountConnectionResponse -> TestTree
responseAcceptEnvironmentAccountConnection =
  res
    "AcceptEnvironmentAccountConnectionResponse"
    "fixture/AcceptEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptEnvironmentAccountConnection)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseUpdateAccountSettings :: UpdateAccountSettingsResponse -> TestTree
responseUpdateAccountSettings =
  res
    "UpdateAccountSettingsResponse"
    "fixture/UpdateAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccountSettings)

responseRejectEnvironmentAccountConnection :: RejectEnvironmentAccountConnectionResponse -> TestTree
responseRejectEnvironmentAccountConnection =
  res
    "RejectEnvironmentAccountConnectionResponse"
    "fixture/RejectEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectEnvironmentAccountConnection)

responseListServiceInstances :: ListServiceInstancesResponse -> TestTree
responseListServiceInstances =
  res
    "ListServiceInstancesResponse"
    "fixture/ListServiceInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceInstances)

responseCancelServicePipelineDeployment :: CancelServicePipelineDeploymentResponse -> TestTree
responseCancelServicePipelineDeployment =
  res
    "CancelServicePipelineDeploymentResponse"
    "fixture/CancelServicePipelineDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelServicePipelineDeployment)

responseCreateServiceTemplateVersion :: CreateServiceTemplateVersionResponse -> TestTree
responseCreateServiceTemplateVersion =
  res
    "CreateServiceTemplateVersionResponse"
    "fixture/CreateServiceTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceTemplateVersion)

responseGetServiceTemplate :: GetServiceTemplateResponse -> TestTree
responseGetServiceTemplate =
  res
    "GetServiceTemplateResponse"
    "fixture/GetServiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceTemplate)

responseCreateEnvironmentTemplateVersion :: CreateEnvironmentTemplateVersionResponse -> TestTree
responseCreateEnvironmentTemplateVersion =
  res
    "CreateEnvironmentTemplateVersionResponse"
    "fixture/CreateEnvironmentTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironmentTemplateVersion)

responseCancelServiceInstanceDeployment :: CancelServiceInstanceDeploymentResponse -> TestTree
responseCancelServiceInstanceDeployment =
  res
    "CancelServiceInstanceDeploymentResponse"
    "fixture/CancelServiceInstanceDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelServiceInstanceDeployment)

responseGetEnvironmentTemplate :: GetEnvironmentTemplateResponse -> TestTree
responseGetEnvironmentTemplate =
  res
    "GetEnvironmentTemplateResponse"
    "fixture/GetEnvironmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironmentTemplate)

responseUpdateServicePipeline :: UpdateServicePipelineResponse -> TestTree
responseUpdateServicePipeline =
  res
    "UpdateServicePipelineResponse"
    "fixture/UpdateServicePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServicePipeline)

responseListServiceTemplateVersions :: ListServiceTemplateVersionsResponse -> TestTree
responseListServiceTemplateVersions =
  res
    "ListServiceTemplateVersionsResponse"
    "fixture/ListServiceTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceTemplateVersions)

responseCreateEnvironmentAccountConnection :: CreateEnvironmentAccountConnectionResponse -> TestTree
responseCreateEnvironmentAccountConnection =
  res
    "CreateEnvironmentAccountConnectionResponse"
    "fixture/CreateEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironmentAccountConnection)

responseListEnvironmentTemplateVersions :: ListEnvironmentTemplateVersionsResponse -> TestTree
responseListEnvironmentTemplateVersions =
  res
    "ListEnvironmentTemplateVersionsResponse"
    "fixture/ListEnvironmentTemplateVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentTemplateVersions)

responseGetEnvironmentTemplateVersion :: GetEnvironmentTemplateVersionResponse -> TestTree
responseGetEnvironmentTemplateVersion =
  res
    "GetEnvironmentTemplateVersionResponse"
    "fixture/GetEnvironmentTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironmentTemplateVersion)

responseCreateServiceTemplate :: CreateServiceTemplateResponse -> TestTree
responseCreateServiceTemplate =
  res
    "CreateServiceTemplateResponse"
    "fixture/CreateServiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateServiceTemplate)

responseGetServiceTemplateVersion :: GetServiceTemplateVersionResponse -> TestTree
responseGetServiceTemplateVersion =
  res
    "GetServiceTemplateVersionResponse"
    "fixture/GetServiceTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceTemplateVersion)

responseCreateEnvironmentTemplate :: CreateEnvironmentTemplateResponse -> TestTree
responseCreateEnvironmentTemplate =
  res
    "CreateEnvironmentTemplateResponse"
    "fixture/CreateEnvironmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironmentTemplate)

responseDeleteEnvironmentTemplate :: DeleteEnvironmentTemplateResponse -> TestTree
responseDeleteEnvironmentTemplate =
  res
    "DeleteEnvironmentTemplateResponse"
    "fixture/DeleteEnvironmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironmentTemplate)

responseUpdateEnvironmentTemplate :: UpdateEnvironmentTemplateResponse -> TestTree
responseUpdateEnvironmentTemplate =
  res
    "UpdateEnvironmentTemplateResponse"
    "fixture/UpdateEnvironmentTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironmentTemplate)

responseGetEnvironmentAccountConnection :: GetEnvironmentAccountConnectionResponse -> TestTree
responseGetEnvironmentAccountConnection =
  res
    "GetEnvironmentAccountConnectionResponse"
    "fixture/GetEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironmentAccountConnection)

responseListEnvironmentTemplates :: ListEnvironmentTemplatesResponse -> TestTree
responseListEnvironmentTemplates =
  res
    "ListEnvironmentTemplatesResponse"
    "fixture/ListEnvironmentTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentTemplates)

responseDeleteServiceTemplate :: DeleteServiceTemplateResponse -> TestTree
responseDeleteServiceTemplate =
  res
    "DeleteServiceTemplateResponse"
    "fixture/DeleteServiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceTemplate)

responseUpdateServiceTemplate :: UpdateServiceTemplateResponse -> TestTree
responseUpdateServiceTemplate =
  res
    "UpdateServiceTemplateResponse"
    "fixture/UpdateServiceTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceTemplate)

responseListServiceTemplates :: ListServiceTemplatesResponse -> TestTree
responseListServiceTemplates =
  res
    "ListServiceTemplatesResponse"
    "fixture/ListServiceTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServiceTemplates)

responseGetAccountSettings :: GetAccountSettingsResponse -> TestTree
responseGetAccountSettings =
  res
    "GetAccountSettingsResponse"
    "fixture/GetAccountSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountSettings)

responseCancelEnvironmentDeployment :: CancelEnvironmentDeploymentResponse -> TestTree
responseCancelEnvironmentDeployment =
  res
    "CancelEnvironmentDeploymentResponse"
    "fixture/CancelEnvironmentDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelEnvironmentDeployment)

responseUpdateServiceInstance :: UpdateServiceInstanceResponse -> TestTree
responseUpdateServiceInstance =
  res
    "UpdateServiceInstanceResponse"
    "fixture/UpdateServiceInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceInstance)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetEnvironment :: GetEnvironmentResponse -> TestTree
responseGetEnvironment =
  res
    "GetEnvironmentResponse"
    "fixture/GetEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironment)

responseListEnvironmentAccountConnections :: ListEnvironmentAccountConnectionsResponse -> TestTree
responseListEnvironmentAccountConnections =
  res
    "ListEnvironmentAccountConnectionsResponse"
    "fixture/ListEnvironmentAccountConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentAccountConnections)

responseGetService :: GetServiceResponse -> TestTree
responseGetService =
  res
    "GetServiceResponse"
    "fixture/GetServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetService)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteEnvironmentAccountConnection :: DeleteEnvironmentAccountConnectionResponse -> TestTree
responseDeleteEnvironmentAccountConnection =
  res
    "DeleteEnvironmentAccountConnectionResponse"
    "fixture/DeleteEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironmentAccountConnection)

responseUpdateEnvironmentAccountConnection :: UpdateEnvironmentAccountConnectionResponse -> TestTree
responseUpdateEnvironmentAccountConnection =
  res
    "UpdateEnvironmentAccountConnectionResponse"
    "fixture/UpdateEnvironmentAccountConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironmentAccountConnection)

responseDeleteServiceTemplateVersion :: DeleteServiceTemplateVersionResponse -> TestTree
responseDeleteServiceTemplateVersion =
  res
    "DeleteServiceTemplateVersionResponse"
    "fixture/DeleteServiceTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteServiceTemplateVersion)

responseUpdateServiceTemplateVersion :: UpdateServiceTemplateVersionResponse -> TestTree
responseUpdateServiceTemplateVersion =
  res
    "UpdateServiceTemplateVersionResponse"
    "fixture/UpdateServiceTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceTemplateVersion)

responseUpdateEnvironmentTemplateVersion :: UpdateEnvironmentTemplateVersionResponse -> TestTree
responseUpdateEnvironmentTemplateVersion =
  res
    "UpdateEnvironmentTemplateVersionResponse"
    "fixture/UpdateEnvironmentTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironmentTemplateVersion)

responseDeleteEnvironmentTemplateVersion :: DeleteEnvironmentTemplateVersionResponse -> TestTree
responseDeleteEnvironmentTemplateVersion =
  res
    "DeleteEnvironmentTemplateVersionResponse"
    "fixture/DeleteEnvironmentTemplateVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironmentTemplateVersion)

responseCreateEnvironment :: CreateEnvironmentResponse -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironment)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)
