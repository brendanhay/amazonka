{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ApiGatewayV2
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         , requestGetDeployments $
--             newGetDeployments
--
--         , requestDeleteAccessLogSettings $
--             newDeleteAccessLogSettings
--
--         , requestGetRouteResponses $
--             newGetRouteResponses
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestGetTags $
--             newGetTags
--
--         , requestGetDomainNames $
--             newGetDomainNames
--
--         , requestReimportApi $
--             newReimportApi
--
--         , requestGetModels $
--             newGetModels
--
--         , requestCreateIntegration $
--             newCreateIntegration
--
--         , requestDeleteStage $
--             newDeleteStage
--
--         , requestUpdateStage $
--             newUpdateStage
--
--         , requestDeleteRouteSettings $
--             newDeleteRouteSettings
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestDeleteRoute $
--             newDeleteRoute
--
--         , requestUpdateRoute $
--             newUpdateRoute
--
--         , requestGetVpcLinks $
--             newGetVpcLinks
--
--         , requestGetIntegrationResponses $
--             newGetIntegrationResponses
--
--         , requestGetIntegration $
--             newGetIntegration
--
--         , requestDeleteDeployment $
--             newDeleteDeployment
--
--         , requestUpdateDeployment $
--             newUpdateDeployment
--
--         , requestDeleteRouteResponse $
--             newDeleteRouteResponse
--
--         , requestUpdateRouteResponse $
--             newUpdateRouteResponse
--
--         , requestGetVpcLink $
--             newGetVpcLink
--
--         , requestResetAuthorizersCache $
--             newResetAuthorizersCache
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestGetIntegrationResponse $
--             newGetIntegrationResponse
--
--         , requestCreateDomainName $
--             newCreateDomainName
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestUpdateModel $
--             newUpdateModel
--
--         , requestCreateRouteResponse $
--             newCreateRouteResponse
--
--         , requestGetStages $
--             newGetStages
--
--         , requestGetModel $
--             newGetModel
--
--         , requestGetApiMappings $
--             newGetApiMappings
--
--         , requestCreateIntegrationResponse $
--             newCreateIntegrationResponse
--
--         , requestGetDomainName $
--             newGetDomainName
--
--         , requestCreateVpcLink $
--             newCreateVpcLink
--
--         , requestGetAuthorizers $
--             newGetAuthorizers
--
--         , requestGetRouteResponse $
--             newGetRouteResponse
--
--         , requestExportApi $
--             newExportApi
--
--         , requestGetRoutes $
--             newGetRoutes
--
--         , requestDeleteCorsConfiguration $
--             newDeleteCorsConfiguration
--
--         , requestDeleteVpcLink $
--             newDeleteVpcLink
--
--         , requestUpdateVpcLink $
--             newUpdateVpcLink
--
--         , requestDeleteIntegrationResponse $
--             newDeleteIntegrationResponse
--
--         , requestUpdateIntegrationResponse $
--             newUpdateIntegrationResponse
--
--         , requestDeleteIntegration $
--             newDeleteIntegration
--
--         , requestUpdateIntegration $
--             newUpdateIntegration
--
--         , requestGetRoute $
--             newGetRoute
--
--         , requestGetAuthorizer $
--             newGetAuthorizer
--
--         , requestGetStage $
--             newGetStage
--
--         , requestGetApiMapping $
--             newGetApiMapping
--
--         , requestImportApi $
--             newImportApi
--
--         , requestGetApis $
--             newGetApis
--
--         , requestUpdateApiMapping $
--             newUpdateApiMapping
--
--         , requestDeleteApiMapping $
--             newDeleteApiMapping
--
--         , requestCreateRoute $
--             newCreateRoute
--
--         , requestCreateAuthorizer $
--             newCreateAuthorizer
--
--         , requestUpdateAuthorizer $
--             newUpdateAuthorizer
--
--         , requestDeleteAuthorizer $
--             newDeleteAuthorizer
--
--         , requestCreateApiMapping $
--             newCreateApiMapping
--
--         , requestDeleteRouteRequestParameter $
--             newDeleteRouteRequestParameter
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateStage $
--             newCreateStage
--
--         , requestGetIntegrations $
--             newGetIntegrations
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDomainName $
--             newUpdateDomainName
--
--         , requestDeleteDomainName $
--             newDeleteDomainName
--
--         , requestGetApi $
--             newGetApi
--
--         , requestDeleteApi $
--             newDeleteApi
--
--         , requestUpdateApi $
--             newUpdateApi
--
--         , requestGetModelTemplate $
--             newGetModelTemplate
--
--           ]

--     , testGroup "response"
--         [ responseCreateApi $
--             newCreateApiResponse
--
--         , responseGetDeployments $
--             newGetDeploymentsResponse
--
--         , responseDeleteAccessLogSettings $
--             newDeleteAccessLogSettingsResponse
--
--         , responseGetRouteResponses $
--             newGetRouteResponsesResponse
--
--         , responseGetDeployment $
--             newGetDeploymentResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseGetDomainNames $
--             newGetDomainNamesResponse
--
--         , responseReimportApi $
--             newReimportApiResponse
--
--         , responseGetModels $
--             newGetModelsResponse
--
--         , responseCreateIntegration $
--             newCreateIntegrationResponse'
--
--         , responseDeleteStage $
--             newDeleteStageResponse
--
--         , responseUpdateStage $
--             newUpdateStageResponse
--
--         , responseDeleteRouteSettings $
--             newDeleteRouteSettingsResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseDeleteRoute $
--             newDeleteRouteResponse'
--
--         , responseUpdateRoute $
--             newUpdateRouteResponse'
--
--         , responseGetVpcLinks $
--             newGetVpcLinksResponse
--
--         , responseGetIntegrationResponses $
--             newGetIntegrationResponsesResponse
--
--         , responseGetIntegration $
--             newGetIntegrationResponse'
--
--         , responseDeleteDeployment $
--             newDeleteDeploymentResponse
--
--         , responseUpdateDeployment $
--             newUpdateDeploymentResponse
--
--         , responseDeleteRouteResponse $
--             newDeleteRouteResponseResponse
--
--         , responseUpdateRouteResponse $
--             newUpdateRouteResponseResponse
--
--         , responseGetVpcLink $
--             newGetVpcLinkResponse
--
--         , responseResetAuthorizersCache $
--             newResetAuthorizersCacheResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseGetIntegrationResponse $
--             newGetIntegrationResponseResponse
--
--         , responseCreateDomainName $
--             newCreateDomainNameResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseUpdateModel $
--             newUpdateModelResponse
--
--         , responseCreateRouteResponse $
--             newCreateRouteResponseResponse
--
--         , responseGetStages $
--             newGetStagesResponse
--
--         , responseGetModel $
--             newGetModelResponse
--
--         , responseGetApiMappings $
--             newGetApiMappingsResponse
--
--         , responseCreateIntegrationResponse $
--             newCreateIntegrationResponseResponse
--
--         , responseGetDomainName $
--             newGetDomainNameResponse
--
--         , responseCreateVpcLink $
--             newCreateVpcLinkResponse
--
--         , responseGetAuthorizers $
--             newGetAuthorizersResponse
--
--         , responseGetRouteResponse $
--             newGetRouteResponseResponse
--
--         , responseExportApi $
--             newExportApiResponse
--
--         , responseGetRoutes $
--             newGetRoutesResponse
--
--         , responseDeleteCorsConfiguration $
--             newDeleteCorsConfigurationResponse
--
--         , responseDeleteVpcLink $
--             newDeleteVpcLinkResponse
--
--         , responseUpdateVpcLink $
--             newUpdateVpcLinkResponse
--
--         , responseDeleteIntegrationResponse $
--             newDeleteIntegrationResponseResponse
--
--         , responseUpdateIntegrationResponse $
--             newUpdateIntegrationResponseResponse
--
--         , responseDeleteIntegration $
--             newDeleteIntegrationResponse'
--
--         , responseUpdateIntegration $
--             newUpdateIntegrationResponse'
--
--         , responseGetRoute $
--             newGetRouteResponse'
--
--         , responseGetAuthorizer $
--             newGetAuthorizerResponse
--
--         , responseGetStage $
--             newGetStageResponse
--
--         , responseGetApiMapping $
--             newGetApiMappingResponse
--
--         , responseImportApi $
--             newImportApiResponse
--
--         , responseGetApis $
--             newGetApisResponse
--
--         , responseUpdateApiMapping $
--             newUpdateApiMappingResponse
--
--         , responseDeleteApiMapping $
--             newDeleteApiMappingResponse
--
--         , responseCreateRoute $
--             newCreateRouteResponse'
--
--         , responseCreateAuthorizer $
--             newCreateAuthorizerResponse
--
--         , responseUpdateAuthorizer $
--             newUpdateAuthorizerResponse
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseCreateApiMapping $
--             newCreateApiMappingResponse
--
--         , responseDeleteRouteRequestParameter $
--             newDeleteRouteRequestParameterResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateStage $
--             newCreateStageResponse
--
--         , responseGetIntegrations $
--             newGetIntegrationsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDomainName $
--             newUpdateDomainNameResponse
--
--         , responseDeleteDomainName $
--             newDeleteDomainNameResponse
--
--         , responseGetApi $
--             newGetApiResponse
--
--         , responseDeleteApi $
--             newDeleteApiResponse
--
--         , responseUpdateApi $
--             newUpdateApiResponse
--
--         , responseGetModelTemplate $
--             newGetModelTemplateResponse
--
--           ]
--     ]

-- Requests

requestCreateApi :: CreateApi -> TestTree
requestCreateApi =
  req
    "CreateApi"
    "fixture/CreateApi.yaml"

requestGetDeployments :: GetDeployments -> TestTree
requestGetDeployments =
  req
    "GetDeployments"
    "fixture/GetDeployments.yaml"

requestDeleteAccessLogSettings :: DeleteAccessLogSettings -> TestTree
requestDeleteAccessLogSettings =
  req
    "DeleteAccessLogSettings"
    "fixture/DeleteAccessLogSettings.yaml"

requestGetRouteResponses :: GetRouteResponses -> TestTree
requestGetRouteResponses =
  req
    "GetRouteResponses"
    "fixture/GetRouteResponses.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestGetDomainNames :: GetDomainNames -> TestTree
requestGetDomainNames =
  req
    "GetDomainNames"
    "fixture/GetDomainNames.yaml"

requestReimportApi :: ReimportApi -> TestTree
requestReimportApi =
  req
    "ReimportApi"
    "fixture/ReimportApi.yaml"

requestGetModels :: GetModels -> TestTree
requestGetModels =
  req
    "GetModels"
    "fixture/GetModels.yaml"

requestCreateIntegration :: CreateIntegration -> TestTree
requestCreateIntegration =
  req
    "CreateIntegration"
    "fixture/CreateIntegration.yaml"

requestDeleteStage :: DeleteStage -> TestTree
requestDeleteStage =
  req
    "DeleteStage"
    "fixture/DeleteStage.yaml"

requestUpdateStage :: UpdateStage -> TestTree
requestUpdateStage =
  req
    "UpdateStage"
    "fixture/UpdateStage.yaml"

requestDeleteRouteSettings :: DeleteRouteSettings -> TestTree
requestDeleteRouteSettings =
  req
    "DeleteRouteSettings"
    "fixture/DeleteRouteSettings.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute =
  req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestUpdateRoute :: UpdateRoute -> TestTree
requestUpdateRoute =
  req
    "UpdateRoute"
    "fixture/UpdateRoute.yaml"

requestGetVpcLinks :: GetVpcLinks -> TestTree
requestGetVpcLinks =
  req
    "GetVpcLinks"
    "fixture/GetVpcLinks.yaml"

requestGetIntegrationResponses :: GetIntegrationResponses -> TestTree
requestGetIntegrationResponses =
  req
    "GetIntegrationResponses"
    "fixture/GetIntegrationResponses.yaml"

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration =
  req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

requestDeleteDeployment :: DeleteDeployment -> TestTree
requestDeleteDeployment =
  req
    "DeleteDeployment"
    "fixture/DeleteDeployment.yaml"

requestUpdateDeployment :: UpdateDeployment -> TestTree
requestUpdateDeployment =
  req
    "UpdateDeployment"
    "fixture/UpdateDeployment.yaml"

requestDeleteRouteResponse :: DeleteRouteResponse -> TestTree
requestDeleteRouteResponse =
  req
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.yaml"

requestUpdateRouteResponse :: UpdateRouteResponse -> TestTree
requestUpdateRouteResponse =
  req
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.yaml"

requestGetVpcLink :: GetVpcLink -> TestTree
requestGetVpcLink =
  req
    "GetVpcLink"
    "fixture/GetVpcLink.yaml"

requestResetAuthorizersCache :: ResetAuthorizersCache -> TestTree
requestResetAuthorizersCache =
  req
    "ResetAuthorizersCache"
    "fixture/ResetAuthorizersCache.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestGetIntegrationResponse :: GetIntegrationResponse -> TestTree
requestGetIntegrationResponse =
  req
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.yaml"

requestCreateDomainName :: CreateDomainName -> TestTree
requestCreateDomainName =
  req
    "CreateDomainName"
    "fixture/CreateDomainName.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel =
  req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestCreateRouteResponse :: CreateRouteResponse -> TestTree
requestCreateRouteResponse =
  req
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.yaml"

requestGetStages :: GetStages -> TestTree
requestGetStages =
  req
    "GetStages"
    "fixture/GetStages.yaml"

requestGetModel :: GetModel -> TestTree
requestGetModel =
  req
    "GetModel"
    "fixture/GetModel.yaml"

requestGetApiMappings :: GetApiMappings -> TestTree
requestGetApiMappings =
  req
    "GetApiMappings"
    "fixture/GetApiMappings.yaml"

requestCreateIntegrationResponse :: CreateIntegrationResponse -> TestTree
requestCreateIntegrationResponse =
  req
    "CreateIntegrationResponse"
    "fixture/CreateIntegrationResponse.yaml"

requestGetDomainName :: GetDomainName -> TestTree
requestGetDomainName =
  req
    "GetDomainName"
    "fixture/GetDomainName.yaml"

requestCreateVpcLink :: CreateVpcLink -> TestTree
requestCreateVpcLink =
  req
    "CreateVpcLink"
    "fixture/CreateVpcLink.yaml"

requestGetAuthorizers :: GetAuthorizers -> TestTree
requestGetAuthorizers =
  req
    "GetAuthorizers"
    "fixture/GetAuthorizers.yaml"

requestGetRouteResponse :: GetRouteResponse -> TestTree
requestGetRouteResponse =
  req
    "GetRouteResponse"
    "fixture/GetRouteResponse.yaml"

requestExportApi :: ExportApi -> TestTree
requestExportApi =
  req
    "ExportApi"
    "fixture/ExportApi.yaml"

requestGetRoutes :: GetRoutes -> TestTree
requestGetRoutes =
  req
    "GetRoutes"
    "fixture/GetRoutes.yaml"

requestDeleteCorsConfiguration :: DeleteCorsConfiguration -> TestTree
requestDeleteCorsConfiguration =
  req
    "DeleteCorsConfiguration"
    "fixture/DeleteCorsConfiguration.yaml"

requestDeleteVpcLink :: DeleteVpcLink -> TestTree
requestDeleteVpcLink =
  req
    "DeleteVpcLink"
    "fixture/DeleteVpcLink.yaml"

requestUpdateVpcLink :: UpdateVpcLink -> TestTree
requestUpdateVpcLink =
  req
    "UpdateVpcLink"
    "fixture/UpdateVpcLink.yaml"

requestDeleteIntegrationResponse :: DeleteIntegrationResponse -> TestTree
requestDeleteIntegrationResponse =
  req
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.yaml"

requestUpdateIntegrationResponse :: UpdateIntegrationResponse -> TestTree
requestUpdateIntegrationResponse =
  req
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.yaml"

requestDeleteIntegration :: DeleteIntegration -> TestTree
requestDeleteIntegration =
  req
    "DeleteIntegration"
    "fixture/DeleteIntegration.yaml"

requestUpdateIntegration :: UpdateIntegration -> TestTree
requestUpdateIntegration =
  req
    "UpdateIntegration"
    "fixture/UpdateIntegration.yaml"

requestGetRoute :: GetRoute -> TestTree
requestGetRoute =
  req
    "GetRoute"
    "fixture/GetRoute.yaml"

requestGetAuthorizer :: GetAuthorizer -> TestTree
requestGetAuthorizer =
  req
    "GetAuthorizer"
    "fixture/GetAuthorizer.yaml"

requestGetStage :: GetStage -> TestTree
requestGetStage =
  req
    "GetStage"
    "fixture/GetStage.yaml"

requestGetApiMapping :: GetApiMapping -> TestTree
requestGetApiMapping =
  req
    "GetApiMapping"
    "fixture/GetApiMapping.yaml"

requestImportApi :: ImportApi -> TestTree
requestImportApi =
  req
    "ImportApi"
    "fixture/ImportApi.yaml"

requestGetApis :: GetApis -> TestTree
requestGetApis =
  req
    "GetApis"
    "fixture/GetApis.yaml"

requestUpdateApiMapping :: UpdateApiMapping -> TestTree
requestUpdateApiMapping =
  req
    "UpdateApiMapping"
    "fixture/UpdateApiMapping.yaml"

requestDeleteApiMapping :: DeleteApiMapping -> TestTree
requestDeleteApiMapping =
  req
    "DeleteApiMapping"
    "fixture/DeleteApiMapping.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute =
  req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer =
  req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer =
  req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer =
  req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestCreateApiMapping :: CreateApiMapping -> TestTree
requestCreateApiMapping =
  req
    "CreateApiMapping"
    "fixture/CreateApiMapping.yaml"

requestDeleteRouteRequestParameter :: DeleteRouteRequestParameter -> TestTree
requestDeleteRouteRequestParameter =
  req
    "DeleteRouteRequestParameter"
    "fixture/DeleteRouteRequestParameter.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateStage :: CreateStage -> TestTree
requestCreateStage =
  req
    "CreateStage"
    "fixture/CreateStage.yaml"

requestGetIntegrations :: GetIntegrations -> TestTree
requestGetIntegrations =
  req
    "GetIntegrations"
    "fixture/GetIntegrations.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateDomainName :: UpdateDomainName -> TestTree
requestUpdateDomainName =
  req
    "UpdateDomainName"
    "fixture/UpdateDomainName.yaml"

requestDeleteDomainName :: DeleteDomainName -> TestTree
requestDeleteDomainName =
  req
    "DeleteDomainName"
    "fixture/DeleteDomainName.yaml"

requestGetApi :: GetApi -> TestTree
requestGetApi =
  req
    "GetApi"
    "fixture/GetApi.yaml"

requestDeleteApi :: DeleteApi -> TestTree
requestDeleteApi =
  req
    "DeleteApi"
    "fixture/DeleteApi.yaml"

requestUpdateApi :: UpdateApi -> TestTree
requestUpdateApi =
  req
    "UpdateApi"
    "fixture/UpdateApi.yaml"

requestGetModelTemplate :: GetModelTemplate -> TestTree
requestGetModelTemplate =
  req
    "GetModelTemplate"
    "fixture/GetModelTemplate.yaml"

-- Responses

responseCreateApi :: CreateApiResponse -> TestTree
responseCreateApi =
  res
    "CreateApiResponse"
    "fixture/CreateApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApi)

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments =
  res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployments)

responseDeleteAccessLogSettings :: DeleteAccessLogSettingsResponse -> TestTree
responseDeleteAccessLogSettings =
  res
    "DeleteAccessLogSettingsResponse"
    "fixture/DeleteAccessLogSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessLogSettings)

responseGetRouteResponses :: GetRouteResponsesResponse -> TestTree
responseGetRouteResponses =
  res
    "GetRouteResponsesResponse"
    "fixture/GetRouteResponsesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRouteResponses)

responseGetDeployment :: GetDeploymentResponse -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployment)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTags)

responseGetDomainNames :: GetDomainNamesResponse -> TestTree
responseGetDomainNames =
  res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainNames)

responseReimportApi :: ReimportApiResponse -> TestTree
responseReimportApi =
  res
    "ReimportApiResponse"
    "fixture/ReimportApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReimportApi)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels =
  res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModels)

responseCreateIntegration :: CreateIntegrationResponse' -> TestTree
responseCreateIntegration =
  res
    "CreateIntegrationResponse"
    "fixture/CreateIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntegration)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage =
  res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStage)

responseUpdateStage :: UpdateStageResponse -> TestTree
responseUpdateStage =
  res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStage)

responseDeleteRouteSettings :: DeleteRouteSettingsResponse -> TestTree
responseDeleteRouteSettings =
  res
    "DeleteRouteSettingsResponse"
    "fixture/DeleteRouteSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteSettings)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseDeleteRoute :: DeleteRouteResponse' -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoute)

responseUpdateRoute :: UpdateRouteResponse' -> TestTree
responseUpdateRoute =
  res
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoute)

responseGetVpcLinks :: GetVpcLinksResponse -> TestTree
responseGetVpcLinks =
  res
    "GetVpcLinksResponse"
    "fixture/GetVpcLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpcLinks)

responseGetIntegrationResponses :: GetIntegrationResponsesResponse -> TestTree
responseGetIntegrationResponses =
  res
    "GetIntegrationResponsesResponse"
    "fixture/GetIntegrationResponsesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegrationResponses)

responseGetIntegration :: GetIntegrationResponse' -> TestTree
responseGetIntegration =
  res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegration)

responseDeleteDeployment :: DeleteDeploymentResponse -> TestTree
responseDeleteDeployment =
  res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeployment)

responseUpdateDeployment :: UpdateDeploymentResponse -> TestTree
responseUpdateDeployment =
  res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeployment)

responseDeleteRouteResponse :: DeleteRouteResponseResponse -> TestTree
responseDeleteRouteResponse =
  res
    "DeleteRouteResponseResponse"
    "fixture/DeleteRouteResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteResponse)

responseUpdateRouteResponse :: UpdateRouteResponseResponse -> TestTree
responseUpdateRouteResponse =
  res
    "UpdateRouteResponseResponse"
    "fixture/UpdateRouteResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRouteResponse)

responseGetVpcLink :: GetVpcLinkResponse -> TestTree
responseGetVpcLink =
  res
    "GetVpcLinkResponse"
    "fixture/GetVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpcLink)

responseResetAuthorizersCache :: ResetAuthorizersCacheResponse -> TestTree
responseResetAuthorizersCache =
  res
    "ResetAuthorizersCacheResponse"
    "fixture/ResetAuthorizersCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetAuthorizersCache)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

responseGetIntegrationResponse :: GetIntegrationResponseResponse -> TestTree
responseGetIntegrationResponse =
  res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegrationResponse)

responseCreateDomainName :: CreateDomainNameResponse -> TestTree
responseCreateDomainName =
  res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainName)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseUpdateModel :: UpdateModelResponse -> TestTree
responseUpdateModel =
  res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModel)

responseCreateRouteResponse :: CreateRouteResponseResponse -> TestTree
responseCreateRouteResponse =
  res
    "CreateRouteResponseResponse"
    "fixture/CreateRouteResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRouteResponse)

responseGetStages :: GetStagesResponse -> TestTree
responseGetStages =
  res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStages)

responseGetModel :: GetModelResponse -> TestTree
responseGetModel =
  res
    "GetModelResponse"
    "fixture/GetModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModel)

responseGetApiMappings :: GetApiMappingsResponse -> TestTree
responseGetApiMappings =
  res
    "GetApiMappingsResponse"
    "fixture/GetApiMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiMappings)

responseCreateIntegrationResponse :: CreateIntegrationResponseResponse -> TestTree
responseCreateIntegrationResponse =
  res
    "CreateIntegrationResponseResponse"
    "fixture/CreateIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntegrationResponse)

responseGetDomainName :: GetDomainNameResponse -> TestTree
responseGetDomainName =
  res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainName)

responseCreateVpcLink :: CreateVpcLinkResponse -> TestTree
responseCreateVpcLink =
  res
    "CreateVpcLinkResponse"
    "fixture/CreateVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcLink)

responseGetAuthorizers :: GetAuthorizersResponse -> TestTree
responseGetAuthorizers =
  res
    "GetAuthorizersResponse"
    "fixture/GetAuthorizersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizers)

responseGetRouteResponse :: GetRouteResponseResponse -> TestTree
responseGetRouteResponse =
  res
    "GetRouteResponseResponse"
    "fixture/GetRouteResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRouteResponse)

responseExportApi :: ExportApiResponse -> TestTree
responseExportApi =
  res
    "ExportApiResponse"
    "fixture/ExportApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportApi)

responseGetRoutes :: GetRoutesResponse -> TestTree
responseGetRoutes =
  res
    "GetRoutesResponse"
    "fixture/GetRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoutes)

responseDeleteCorsConfiguration :: DeleteCorsConfigurationResponse -> TestTree
responseDeleteCorsConfiguration =
  res
    "DeleteCorsConfigurationResponse"
    "fixture/DeleteCorsConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCorsConfiguration)

responseDeleteVpcLink :: DeleteVpcLinkResponse -> TestTree
responseDeleteVpcLink =
  res
    "DeleteVpcLinkResponse"
    "fixture/DeleteVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcLink)

responseUpdateVpcLink :: UpdateVpcLinkResponse -> TestTree
responseUpdateVpcLink =
  res
    "UpdateVpcLinkResponse"
    "fixture/UpdateVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVpcLink)

responseDeleteIntegrationResponse :: DeleteIntegrationResponseResponse -> TestTree
responseDeleteIntegrationResponse =
  res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegrationResponse)

responseUpdateIntegrationResponse :: UpdateIntegrationResponseResponse -> TestTree
responseUpdateIntegrationResponse =
  res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIntegrationResponse)

responseDeleteIntegration :: DeleteIntegrationResponse' -> TestTree
responseDeleteIntegration =
  res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegration)

responseUpdateIntegration :: UpdateIntegrationResponse' -> TestTree
responseUpdateIntegration =
  res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIntegration)

responseGetRoute :: GetRouteResponse' -> TestTree
responseGetRoute =
  res
    "GetRouteResponse"
    "fixture/GetRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoute)

responseGetAuthorizer :: GetAuthorizerResponse -> TestTree
responseGetAuthorizer =
  res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizer)

responseGetStage :: GetStageResponse -> TestTree
responseGetStage =
  res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStage)

responseGetApiMapping :: GetApiMappingResponse -> TestTree
responseGetApiMapping =
  res
    "GetApiMappingResponse"
    "fixture/GetApiMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiMapping)

responseImportApi :: ImportApiResponse -> TestTree
responseImportApi =
  res
    "ImportApiResponse"
    "fixture/ImportApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportApi)

responseGetApis :: GetApisResponse -> TestTree
responseGetApis =
  res
    "GetApisResponse"
    "fixture/GetApisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApis)

responseUpdateApiMapping :: UpdateApiMappingResponse -> TestTree
responseUpdateApiMapping =
  res
    "UpdateApiMappingResponse"
    "fixture/UpdateApiMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiMapping)

responseDeleteApiMapping :: DeleteApiMappingResponse -> TestTree
responseDeleteApiMapping =
  res
    "DeleteApiMappingResponse"
    "fixture/DeleteApiMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiMapping)

responseCreateRoute :: CreateRouteResponse' -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoute)

responseCreateAuthorizer :: CreateAuthorizerResponse -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuthorizer)

responseUpdateAuthorizer :: UpdateAuthorizerResponse -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuthorizer)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuthorizer)

responseCreateApiMapping :: CreateApiMappingResponse -> TestTree
responseCreateApiMapping =
  res
    "CreateApiMappingResponse"
    "fixture/CreateApiMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiMapping)

responseDeleteRouteRequestParameter :: DeleteRouteRequestParameterResponse -> TestTree
responseDeleteRouteRequestParameter =
  res
    "DeleteRouteRequestParameterResponse"
    "fixture/DeleteRouteRequestParameterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteRequestParameter)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateStage :: CreateStageResponse -> TestTree
responseCreateStage =
  res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStage)

responseGetIntegrations :: GetIntegrationsResponse -> TestTree
responseGetIntegrations =
  res
    "GetIntegrationsResponse"
    "fixture/GetIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegrations)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateDomainName :: UpdateDomainNameResponse -> TestTree
responseUpdateDomainName =
  res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainName)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName =
  res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainName)

responseGetApi :: GetApiResponse -> TestTree
responseGetApi =
  res
    "GetApiResponse"
    "fixture/GetApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApi)

responseDeleteApi :: DeleteApiResponse -> TestTree
responseDeleteApi =
  res
    "DeleteApiResponse"
    "fixture/DeleteApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApi)

responseUpdateApi :: UpdateApiResponse -> TestTree
responseUpdateApi =
  res
    "UpdateApiResponse"
    "fixture/UpdateApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApi)

responseGetModelTemplate :: GetModelTemplateResponse -> TestTree
responseGetModelTemplate =
  res
    "GetModelTemplateResponse"
    "fixture/GetModelTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModelTemplate)
