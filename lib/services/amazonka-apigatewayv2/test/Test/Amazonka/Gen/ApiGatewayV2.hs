{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ApiGatewayV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ApiGatewayV2 where

import Amazonka.ApiGatewayV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.ApiGatewayV2.Internal
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
--         [ requestCreateApi $
--             newCreateApi
--
--         , requestCreateApiMapping $
--             newCreateApiMapping
--
--         , requestCreateAuthorizer $
--             newCreateAuthorizer
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestCreateDomainName $
--             newCreateDomainName
--
--         , requestCreateIntegration $
--             newCreateIntegration
--
--         , requestCreateIntegrationResponse $
--             newCreateIntegrationResponse
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestCreateRoute $
--             newCreateRoute
--
--         , requestCreateRouteResponse $
--             newCreateRouteResponse
--
--         , requestCreateStage $
--             newCreateStage
--
--         , requestCreateVpcLink $
--             newCreateVpcLink
--
--         , requestDeleteAccessLogSettings $
--             newDeleteAccessLogSettings
--
--         , requestDeleteApi $
--             newDeleteApi
--
--         , requestDeleteApiMapping $
--             newDeleteApiMapping
--
--         , requestDeleteAuthorizer $
--             newDeleteAuthorizer
--
--         , requestDeleteCorsConfiguration $
--             newDeleteCorsConfiguration
--
--         , requestDeleteDeployment $
--             newDeleteDeployment
--
--         , requestDeleteDomainName $
--             newDeleteDomainName
--
--         , requestDeleteIntegration $
--             newDeleteIntegration
--
--         , requestDeleteIntegrationResponse $
--             newDeleteIntegrationResponse
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestDeleteRoute $
--             newDeleteRoute
--
--         , requestDeleteRouteRequestParameter $
--             newDeleteRouteRequestParameter
--
--         , requestDeleteRouteResponse $
--             newDeleteRouteResponse
--
--         , requestDeleteRouteSettings $
--             newDeleteRouteSettings
--
--         , requestDeleteStage $
--             newDeleteStage
--
--         , requestDeleteVpcLink $
--             newDeleteVpcLink
--
--         , requestExportApi $
--             newExportApi
--
--         , requestGetApi $
--             newGetApi
--
--         , requestGetApiMapping $
--             newGetApiMapping
--
--         , requestGetApiMappings $
--             newGetApiMappings
--
--         , requestGetApis $
--             newGetApis
--
--         , requestGetAuthorizer $
--             newGetAuthorizer
--
--         , requestGetAuthorizers $
--             newGetAuthorizers
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestGetDeployments $
--             newGetDeployments
--
--         , requestGetDomainName $
--             newGetDomainName
--
--         , requestGetDomainNames $
--             newGetDomainNames
--
--         , requestGetIntegration $
--             newGetIntegration
--
--         , requestGetIntegrationResponse $
--             newGetIntegrationResponse
--
--         , requestGetIntegrationResponses $
--             newGetIntegrationResponses
--
--         , requestGetIntegrations $
--             newGetIntegrations
--
--         , requestGetModel $
--             newGetModel
--
--         , requestGetModelTemplate $
--             newGetModelTemplate
--
--         , requestGetModels $
--             newGetModels
--
--         , requestGetRoute $
--             newGetRoute
--
--         , requestGetRouteResponse $
--             newGetRouteResponse
--
--         , requestGetRouteResponses $
--             newGetRouteResponses
--
--         , requestGetRoutes $
--             newGetRoutes
--
--         , requestGetStage $
--             newGetStage
--
--         , requestGetStages $
--             newGetStages
--
--         , requestGetTags $
--             newGetTags
--
--         , requestGetVpcLink $
--             newGetVpcLink
--
--         , requestGetVpcLinks $
--             newGetVpcLinks
--
--         , requestImportApi $
--             newImportApi
--
--         , requestReimportApi $
--             newReimportApi
--
--         , requestResetAuthorizersCache $
--             newResetAuthorizersCache
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApi $
--             newUpdateApi
--
--         , requestUpdateApiMapping $
--             newUpdateApiMapping
--
--         , requestUpdateAuthorizer $
--             newUpdateAuthorizer
--
--         , requestUpdateDeployment $
--             newUpdateDeployment
--
--         , requestUpdateDomainName $
--             newUpdateDomainName
--
--         , requestUpdateIntegration $
--             newUpdateIntegration
--
--         , requestUpdateIntegrationResponse $
--             newUpdateIntegrationResponse
--
--         , requestUpdateModel $
--             newUpdateModel
--
--         , requestUpdateRoute $
--             newUpdateRoute
--
--         , requestUpdateRouteResponse $
--             newUpdateRouteResponse
--
--         , requestUpdateStage $
--             newUpdateStage
--
--         , requestUpdateVpcLink $
--             newUpdateVpcLink
--
--           ]

--     , testGroup "response"
--         [ responseCreateApi $
--             newCreateApiResponse
--
--         , responseCreateApiMapping $
--             newCreateApiMappingResponse
--
--         , responseCreateAuthorizer $
--             newCreateAuthorizerResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseCreateDomainName $
--             newCreateDomainNameResponse
--
--         , responseCreateIntegration $
--             newCreateIntegrationResponse'
--
--         , responseCreateIntegrationResponse $
--             newCreateIntegrationResponseResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseCreateRoute $
--             newCreateRouteResponse'
--
--         , responseCreateRouteResponse $
--             newCreateRouteResponseResponse
--
--         , responseCreateStage $
--             newCreateStageResponse
--
--         , responseCreateVpcLink $
--             newCreateVpcLinkResponse
--
--         , responseDeleteAccessLogSettings $
--             newDeleteAccessLogSettingsResponse
--
--         , responseDeleteApi $
--             newDeleteApiResponse
--
--         , responseDeleteApiMapping $
--             newDeleteApiMappingResponse
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseDeleteCorsConfiguration $
--             newDeleteCorsConfigurationResponse
--
--         , responseDeleteDeployment $
--             newDeleteDeploymentResponse
--
--         , responseDeleteDomainName $
--             newDeleteDomainNameResponse
--
--         , responseDeleteIntegration $
--             newDeleteIntegrationResponse'
--
--         , responseDeleteIntegrationResponse $
--             newDeleteIntegrationResponseResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseDeleteRoute $
--             newDeleteRouteResponse'
--
--         , responseDeleteRouteRequestParameter $
--             newDeleteRouteRequestParameterResponse
--
--         , responseDeleteRouteResponse $
--             newDeleteRouteResponseResponse
--
--         , responseDeleteRouteSettings $
--             newDeleteRouteSettingsResponse
--
--         , responseDeleteStage $
--             newDeleteStageResponse
--
--         , responseDeleteVpcLink $
--             newDeleteVpcLinkResponse
--
--         , responseExportApi $
--             newExportApiResponse
--
--         , responseGetApi $
--             newGetApiResponse
--
--         , responseGetApiMapping $
--             newGetApiMappingResponse
--
--         , responseGetApiMappings $
--             newGetApiMappingsResponse
--
--         , responseGetApis $
--             newGetApisResponse
--
--         , responseGetAuthorizer $
--             newGetAuthorizerResponse
--
--         , responseGetAuthorizers $
--             newGetAuthorizersResponse
--
--         , responseGetDeployment $
--             newGetDeploymentResponse
--
--         , responseGetDeployments $
--             newGetDeploymentsResponse
--
--         , responseGetDomainName $
--             newGetDomainNameResponse
--
--         , responseGetDomainNames $
--             newGetDomainNamesResponse
--
--         , responseGetIntegration $
--             newGetIntegrationResponse'
--
--         , responseGetIntegrationResponse $
--             newGetIntegrationResponseResponse
--
--         , responseGetIntegrationResponses $
--             newGetIntegrationResponsesResponse
--
--         , responseGetIntegrations $
--             newGetIntegrationsResponse
--
--         , responseGetModel $
--             newGetModelResponse
--
--         , responseGetModelTemplate $
--             newGetModelTemplateResponse
--
--         , responseGetModels $
--             newGetModelsResponse
--
--         , responseGetRoute $
--             newGetRouteResponse'
--
--         , responseGetRouteResponse $
--             newGetRouteResponseResponse
--
--         , responseGetRouteResponses $
--             newGetRouteResponsesResponse
--
--         , responseGetRoutes $
--             newGetRoutesResponse
--
--         , responseGetStage $
--             newGetStageResponse
--
--         , responseGetStages $
--             newGetStagesResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseGetVpcLink $
--             newGetVpcLinkResponse
--
--         , responseGetVpcLinks $
--             newGetVpcLinksResponse
--
--         , responseImportApi $
--             newImportApiResponse
--
--         , responseReimportApi $
--             newReimportApiResponse
--
--         , responseResetAuthorizersCache $
--             newResetAuthorizersCacheResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApi $
--             newUpdateApiResponse
--
--         , responseUpdateApiMapping $
--             newUpdateApiMappingResponse
--
--         , responseUpdateAuthorizer $
--             newUpdateAuthorizerResponse
--
--         , responseUpdateDeployment $
--             newUpdateDeploymentResponse
--
--         , responseUpdateDomainName $
--             newUpdateDomainNameResponse
--
--         , responseUpdateIntegration $
--             newUpdateIntegrationResponse'
--
--         , responseUpdateIntegrationResponse $
--             newUpdateIntegrationResponseResponse
--
--         , responseUpdateModel $
--             newUpdateModelResponse
--
--         , responseUpdateRoute $
--             newUpdateRouteResponse'
--
--         , responseUpdateRouteResponse $
--             newUpdateRouteResponseResponse
--
--         , responseUpdateStage $
--             newUpdateStageResponse
--
--         , responseUpdateVpcLink $
--             newUpdateVpcLinkResponse
--
--           ]
--     ]

-- Requests

requestCreateApi :: CreateApi -> TestTree
requestCreateApi =
  req
    "CreateApi"
    "fixture/CreateApi.yaml"

requestCreateApiMapping :: CreateApiMapping -> TestTree
requestCreateApiMapping =
  req
    "CreateApiMapping"
    "fixture/CreateApiMapping.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer =
  req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestCreateDomainName :: CreateDomainName -> TestTree
requestCreateDomainName =
  req
    "CreateDomainName"
    "fixture/CreateDomainName.yaml"

requestCreateIntegration :: CreateIntegration -> TestTree
requestCreateIntegration =
  req
    "CreateIntegration"
    "fixture/CreateIntegration.yaml"

requestCreateIntegrationResponse :: CreateIntegrationResponse -> TestTree
requestCreateIntegrationResponse =
  req
    "CreateIntegrationResponse"
    "fixture/CreateIntegrationResponse.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute =
  req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestCreateRouteResponse :: CreateRouteResponse -> TestTree
requestCreateRouteResponse =
  req
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.yaml"

requestCreateStage :: CreateStage -> TestTree
requestCreateStage =
  req
    "CreateStage"
    "fixture/CreateStage.yaml"

requestCreateVpcLink :: CreateVpcLink -> TestTree
requestCreateVpcLink =
  req
    "CreateVpcLink"
    "fixture/CreateVpcLink.yaml"

requestDeleteAccessLogSettings :: DeleteAccessLogSettings -> TestTree
requestDeleteAccessLogSettings =
  req
    "DeleteAccessLogSettings"
    "fixture/DeleteAccessLogSettings.yaml"

requestDeleteApi :: DeleteApi -> TestTree
requestDeleteApi =
  req
    "DeleteApi"
    "fixture/DeleteApi.yaml"

requestDeleteApiMapping :: DeleteApiMapping -> TestTree
requestDeleteApiMapping =
  req
    "DeleteApiMapping"
    "fixture/DeleteApiMapping.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer =
  req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestDeleteCorsConfiguration :: DeleteCorsConfiguration -> TestTree
requestDeleteCorsConfiguration =
  req
    "DeleteCorsConfiguration"
    "fixture/DeleteCorsConfiguration.yaml"

requestDeleteDeployment :: DeleteDeployment -> TestTree
requestDeleteDeployment =
  req
    "DeleteDeployment"
    "fixture/DeleteDeployment.yaml"

requestDeleteDomainName :: DeleteDomainName -> TestTree
requestDeleteDomainName =
  req
    "DeleteDomainName"
    "fixture/DeleteDomainName.yaml"

requestDeleteIntegration :: DeleteIntegration -> TestTree
requestDeleteIntegration =
  req
    "DeleteIntegration"
    "fixture/DeleteIntegration.yaml"

requestDeleteIntegrationResponse :: DeleteIntegrationResponse -> TestTree
requestDeleteIntegrationResponse =
  req
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute =
  req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestDeleteRouteRequestParameter :: DeleteRouteRequestParameter -> TestTree
requestDeleteRouteRequestParameter =
  req
    "DeleteRouteRequestParameter"
    "fixture/DeleteRouteRequestParameter.yaml"

requestDeleteRouteResponse :: DeleteRouteResponse -> TestTree
requestDeleteRouteResponse =
  req
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.yaml"

requestDeleteRouteSettings :: DeleteRouteSettings -> TestTree
requestDeleteRouteSettings =
  req
    "DeleteRouteSettings"
    "fixture/DeleteRouteSettings.yaml"

requestDeleteStage :: DeleteStage -> TestTree
requestDeleteStage =
  req
    "DeleteStage"
    "fixture/DeleteStage.yaml"

requestDeleteVpcLink :: DeleteVpcLink -> TestTree
requestDeleteVpcLink =
  req
    "DeleteVpcLink"
    "fixture/DeleteVpcLink.yaml"

requestExportApi :: ExportApi -> TestTree
requestExportApi =
  req
    "ExportApi"
    "fixture/ExportApi.yaml"

requestGetApi :: GetApi -> TestTree
requestGetApi =
  req
    "GetApi"
    "fixture/GetApi.yaml"

requestGetApiMapping :: GetApiMapping -> TestTree
requestGetApiMapping =
  req
    "GetApiMapping"
    "fixture/GetApiMapping.yaml"

requestGetApiMappings :: GetApiMappings -> TestTree
requestGetApiMappings =
  req
    "GetApiMappings"
    "fixture/GetApiMappings.yaml"

requestGetApis :: GetApis -> TestTree
requestGetApis =
  req
    "GetApis"
    "fixture/GetApis.yaml"

requestGetAuthorizer :: GetAuthorizer -> TestTree
requestGetAuthorizer =
  req
    "GetAuthorizer"
    "fixture/GetAuthorizer.yaml"

requestGetAuthorizers :: GetAuthorizers -> TestTree
requestGetAuthorizers =
  req
    "GetAuthorizers"
    "fixture/GetAuthorizers.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestGetDeployments :: GetDeployments -> TestTree
requestGetDeployments =
  req
    "GetDeployments"
    "fixture/GetDeployments.yaml"

requestGetDomainName :: GetDomainName -> TestTree
requestGetDomainName =
  req
    "GetDomainName"
    "fixture/GetDomainName.yaml"

requestGetDomainNames :: GetDomainNames -> TestTree
requestGetDomainNames =
  req
    "GetDomainNames"
    "fixture/GetDomainNames.yaml"

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration =
  req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

requestGetIntegrationResponse :: GetIntegrationResponse -> TestTree
requestGetIntegrationResponse =
  req
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.yaml"

requestGetIntegrationResponses :: GetIntegrationResponses -> TestTree
requestGetIntegrationResponses =
  req
    "GetIntegrationResponses"
    "fixture/GetIntegrationResponses.yaml"

requestGetIntegrations :: GetIntegrations -> TestTree
requestGetIntegrations =
  req
    "GetIntegrations"
    "fixture/GetIntegrations.yaml"

requestGetModel :: GetModel -> TestTree
requestGetModel =
  req
    "GetModel"
    "fixture/GetModel.yaml"

requestGetModelTemplate :: GetModelTemplate -> TestTree
requestGetModelTemplate =
  req
    "GetModelTemplate"
    "fixture/GetModelTemplate.yaml"

requestGetModels :: GetModels -> TestTree
requestGetModels =
  req
    "GetModels"
    "fixture/GetModels.yaml"

requestGetRoute :: GetRoute -> TestTree
requestGetRoute =
  req
    "GetRoute"
    "fixture/GetRoute.yaml"

requestGetRouteResponse :: GetRouteResponse -> TestTree
requestGetRouteResponse =
  req
    "GetRouteResponse"
    "fixture/GetRouteResponse.yaml"

requestGetRouteResponses :: GetRouteResponses -> TestTree
requestGetRouteResponses =
  req
    "GetRouteResponses"
    "fixture/GetRouteResponses.yaml"

requestGetRoutes :: GetRoutes -> TestTree
requestGetRoutes =
  req
    "GetRoutes"
    "fixture/GetRoutes.yaml"

requestGetStage :: GetStage -> TestTree
requestGetStage =
  req
    "GetStage"
    "fixture/GetStage.yaml"

requestGetStages :: GetStages -> TestTree
requestGetStages =
  req
    "GetStages"
    "fixture/GetStages.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestGetVpcLink :: GetVpcLink -> TestTree
requestGetVpcLink =
  req
    "GetVpcLink"
    "fixture/GetVpcLink.yaml"

requestGetVpcLinks :: GetVpcLinks -> TestTree
requestGetVpcLinks =
  req
    "GetVpcLinks"
    "fixture/GetVpcLinks.yaml"

requestImportApi :: ImportApi -> TestTree
requestImportApi =
  req
    "ImportApi"
    "fixture/ImportApi.yaml"

requestReimportApi :: ReimportApi -> TestTree
requestReimportApi =
  req
    "ReimportApi"
    "fixture/ReimportApi.yaml"

requestResetAuthorizersCache :: ResetAuthorizersCache -> TestTree
requestResetAuthorizersCache =
  req
    "ResetAuthorizersCache"
    "fixture/ResetAuthorizersCache.yaml"

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

requestUpdateApi :: UpdateApi -> TestTree
requestUpdateApi =
  req
    "UpdateApi"
    "fixture/UpdateApi.yaml"

requestUpdateApiMapping :: UpdateApiMapping -> TestTree
requestUpdateApiMapping =
  req
    "UpdateApiMapping"
    "fixture/UpdateApiMapping.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer =
  req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestUpdateDeployment :: UpdateDeployment -> TestTree
requestUpdateDeployment =
  req
    "UpdateDeployment"
    "fixture/UpdateDeployment.yaml"

requestUpdateDomainName :: UpdateDomainName -> TestTree
requestUpdateDomainName =
  req
    "UpdateDomainName"
    "fixture/UpdateDomainName.yaml"

requestUpdateIntegration :: UpdateIntegration -> TestTree
requestUpdateIntegration =
  req
    "UpdateIntegration"
    "fixture/UpdateIntegration.yaml"

requestUpdateIntegrationResponse :: UpdateIntegrationResponse -> TestTree
requestUpdateIntegrationResponse =
  req
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.yaml"

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel =
  req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestUpdateRoute :: UpdateRoute -> TestTree
requestUpdateRoute =
  req
    "UpdateRoute"
    "fixture/UpdateRoute.yaml"

requestUpdateRouteResponse :: UpdateRouteResponse -> TestTree
requestUpdateRouteResponse =
  req
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.yaml"

requestUpdateStage :: UpdateStage -> TestTree
requestUpdateStage =
  req
    "UpdateStage"
    "fixture/UpdateStage.yaml"

requestUpdateVpcLink :: UpdateVpcLink -> TestTree
requestUpdateVpcLink =
  req
    "UpdateVpcLink"
    "fixture/UpdateVpcLink.yaml"

-- Responses

responseCreateApi :: CreateApiResponse -> TestTree
responseCreateApi =
  res
    "CreateApiResponse"
    "fixture/CreateApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApi)

responseCreateApiMapping :: CreateApiMappingResponse -> TestTree
responseCreateApiMapping =
  res
    "CreateApiMappingResponse"
    "fixture/CreateApiMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiMapping)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuthorizer)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseCreateDomainName :: CreateDomainNameResponse -> TestTree
responseCreateDomainName =
  res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainName)

responseCreateIntegration :: CreateIntegrationResponse' -> TestTree
responseCreateIntegration =
  res
    "CreateIntegrationResponse"
    "fixture/CreateIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntegration)

responseCreateIntegrationResponse :: CreateIntegrationResponseResponse -> TestTree
responseCreateIntegrationResponse =
  res
    "CreateIntegrationResponseResponse"
    "fixture/CreateIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntegrationResponse)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

responseCreateRoute :: CreateRouteResponse' -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoute)

responseCreateRouteResponse :: CreateRouteResponseResponse -> TestTree
responseCreateRouteResponse =
  res
    "CreateRouteResponseResponse"
    "fixture/CreateRouteResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRouteResponse)

responseCreateStage :: CreateStageResponse -> TestTree
responseCreateStage =
  res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStage)

responseCreateVpcLink :: CreateVpcLinkResponse -> TestTree
responseCreateVpcLink =
  res
    "CreateVpcLinkResponse"
    "fixture/CreateVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcLink)

responseDeleteAccessLogSettings :: DeleteAccessLogSettingsResponse -> TestTree
responseDeleteAccessLogSettings =
  res
    "DeleteAccessLogSettingsResponse"
    "fixture/DeleteAccessLogSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessLogSettings)

responseDeleteApi :: DeleteApiResponse -> TestTree
responseDeleteApi =
  res
    "DeleteApiResponse"
    "fixture/DeleteApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApi)

responseDeleteApiMapping :: DeleteApiMappingResponse -> TestTree
responseDeleteApiMapping =
  res
    "DeleteApiMappingResponse"
    "fixture/DeleteApiMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiMapping)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuthorizer)

responseDeleteCorsConfiguration :: DeleteCorsConfigurationResponse -> TestTree
responseDeleteCorsConfiguration =
  res
    "DeleteCorsConfigurationResponse"
    "fixture/DeleteCorsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCorsConfiguration)

responseDeleteDeployment :: DeleteDeploymentResponse -> TestTree
responseDeleteDeployment =
  res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeployment)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName =
  res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainName)

responseDeleteIntegration :: DeleteIntegrationResponse' -> TestTree
responseDeleteIntegration =
  res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegration)

responseDeleteIntegrationResponse :: DeleteIntegrationResponseResponse -> TestTree
responseDeleteIntegrationResponse =
  res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegrationResponse)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseDeleteRoute :: DeleteRouteResponse' -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoute)

responseDeleteRouteRequestParameter :: DeleteRouteRequestParameterResponse -> TestTree
responseDeleteRouteRequestParameter =
  res
    "DeleteRouteRequestParameterResponse"
    "fixture/DeleteRouteRequestParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteRequestParameter)

responseDeleteRouteResponse :: DeleteRouteResponseResponse -> TestTree
responseDeleteRouteResponse =
  res
    "DeleteRouteResponseResponse"
    "fixture/DeleteRouteResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteResponse)

responseDeleteRouteSettings :: DeleteRouteSettingsResponse -> TestTree
responseDeleteRouteSettings =
  res
    "DeleteRouteSettingsResponse"
    "fixture/DeleteRouteSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteSettings)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage =
  res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStage)

responseDeleteVpcLink :: DeleteVpcLinkResponse -> TestTree
responseDeleteVpcLink =
  res
    "DeleteVpcLinkResponse"
    "fixture/DeleteVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcLink)

responseExportApi :: ExportApiResponse -> TestTree
responseExportApi =
  res
    "ExportApiResponse"
    "fixture/ExportApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportApi)

responseGetApi :: GetApiResponse -> TestTree
responseGetApi =
  res
    "GetApiResponse"
    "fixture/GetApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApi)

responseGetApiMapping :: GetApiMappingResponse -> TestTree
responseGetApiMapping =
  res
    "GetApiMappingResponse"
    "fixture/GetApiMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiMapping)

responseGetApiMappings :: GetApiMappingsResponse -> TestTree
responseGetApiMappings =
  res
    "GetApiMappingsResponse"
    "fixture/GetApiMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiMappings)

responseGetApis :: GetApisResponse -> TestTree
responseGetApis =
  res
    "GetApisResponse"
    "fixture/GetApisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApis)

responseGetAuthorizer :: GetAuthorizerResponse -> TestTree
responseGetAuthorizer =
  res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizer)

responseGetAuthorizers :: GetAuthorizersResponse -> TestTree
responseGetAuthorizers =
  res
    "GetAuthorizersResponse"
    "fixture/GetAuthorizersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizers)

responseGetDeployment :: GetDeploymentResponse -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployment)

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments =
  res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployments)

responseGetDomainName :: GetDomainNameResponse -> TestTree
responseGetDomainName =
  res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainName)

responseGetDomainNames :: GetDomainNamesResponse -> TestTree
responseGetDomainNames =
  res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainNames)

responseGetIntegration :: GetIntegrationResponse' -> TestTree
responseGetIntegration =
  res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegration)

responseGetIntegrationResponse :: GetIntegrationResponseResponse -> TestTree
responseGetIntegrationResponse =
  res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegrationResponse)

responseGetIntegrationResponses :: GetIntegrationResponsesResponse -> TestTree
responseGetIntegrationResponses =
  res
    "GetIntegrationResponsesResponse"
    "fixture/GetIntegrationResponsesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegrationResponses)

responseGetIntegrations :: GetIntegrationsResponse -> TestTree
responseGetIntegrations =
  res
    "GetIntegrationsResponse"
    "fixture/GetIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegrations)

responseGetModel :: GetModelResponse -> TestTree
responseGetModel =
  res
    "GetModelResponse"
    "fixture/GetModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModel)

responseGetModelTemplate :: GetModelTemplateResponse -> TestTree
responseGetModelTemplate =
  res
    "GetModelTemplateResponse"
    "fixture/GetModelTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModelTemplate)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels =
  res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModels)

responseGetRoute :: GetRouteResponse' -> TestTree
responseGetRoute =
  res
    "GetRouteResponse"
    "fixture/GetRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoute)

responseGetRouteResponse :: GetRouteResponseResponse -> TestTree
responseGetRouteResponse =
  res
    "GetRouteResponseResponse"
    "fixture/GetRouteResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRouteResponse)

responseGetRouteResponses :: GetRouteResponsesResponse -> TestTree
responseGetRouteResponses =
  res
    "GetRouteResponsesResponse"
    "fixture/GetRouteResponsesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRouteResponses)

responseGetRoutes :: GetRoutesResponse -> TestTree
responseGetRoutes =
  res
    "GetRoutesResponse"
    "fixture/GetRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoutes)

responseGetStage :: GetStageResponse -> TestTree
responseGetStage =
  res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStage)

responseGetStages :: GetStagesResponse -> TestTree
responseGetStages =
  res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStages)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTags)

responseGetVpcLink :: GetVpcLinkResponse -> TestTree
responseGetVpcLink =
  res
    "GetVpcLinkResponse"
    "fixture/GetVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpcLink)

responseGetVpcLinks :: GetVpcLinksResponse -> TestTree
responseGetVpcLinks =
  res
    "GetVpcLinksResponse"
    "fixture/GetVpcLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpcLinks)

responseImportApi :: ImportApiResponse -> TestTree
responseImportApi =
  res
    "ImportApiResponse"
    "fixture/ImportApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportApi)

responseReimportApi :: ReimportApiResponse -> TestTree
responseReimportApi =
  res
    "ReimportApiResponse"
    "fixture/ReimportApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReimportApi)

responseResetAuthorizersCache :: ResetAuthorizersCacheResponse -> TestTree
responseResetAuthorizersCache =
  res
    "ResetAuthorizersCacheResponse"
    "fixture/ResetAuthorizersCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetAuthorizersCache)

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

responseUpdateApi :: UpdateApiResponse -> TestTree
responseUpdateApi =
  res
    "UpdateApiResponse"
    "fixture/UpdateApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApi)

responseUpdateApiMapping :: UpdateApiMappingResponse -> TestTree
responseUpdateApiMapping =
  res
    "UpdateApiMappingResponse"
    "fixture/UpdateApiMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiMapping)

responseUpdateAuthorizer :: UpdateAuthorizerResponse -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuthorizer)

responseUpdateDeployment :: UpdateDeploymentResponse -> TestTree
responseUpdateDeployment =
  res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeployment)

responseUpdateDomainName :: UpdateDomainNameResponse -> TestTree
responseUpdateDomainName =
  res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainName)

responseUpdateIntegration :: UpdateIntegrationResponse' -> TestTree
responseUpdateIntegration =
  res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIntegration)

responseUpdateIntegrationResponse :: UpdateIntegrationResponseResponse -> TestTree
responseUpdateIntegrationResponse =
  res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIntegrationResponse)

responseUpdateModel :: UpdateModelResponse -> TestTree
responseUpdateModel =
  res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModel)

responseUpdateRoute :: UpdateRouteResponse' -> TestTree
responseUpdateRoute =
  res
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoute)

responseUpdateRouteResponse :: UpdateRouteResponseResponse -> TestTree
responseUpdateRouteResponse =
  res
    "UpdateRouteResponseResponse"
    "fixture/UpdateRouteResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRouteResponse)

responseUpdateStage :: UpdateStageResponse -> TestTree
responseUpdateStage =
  res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStage)

responseUpdateVpcLink :: UpdateVpcLinkResponse -> TestTree
responseUpdateVpcLink =
  res
    "UpdateVpcLinkResponse"
    "fixture/UpdateVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVpcLink)
