{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.APIGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.APIGateway where

import Amazonka.APIGateway
import qualified Data.Proxy as Proxy
import Test.Amazonka.APIGateway.Internal
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
--         [ requestCreateApiKey $
--             newCreateApiKey
--
--         , requestCreateAuthorizer $
--             newCreateAuthorizer
--
--         , requestCreateBasePathMapping $
--             newCreateBasePathMapping
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestCreateDocumentationPart $
--             newCreateDocumentationPart
--
--         , requestCreateDocumentationVersion $
--             newCreateDocumentationVersion
--
--         , requestCreateDomainName $
--             newCreateDomainName
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestCreateRequestValidator $
--             newCreateRequestValidator
--
--         , requestCreateResource $
--             newCreateResource
--
--         , requestCreateRestApi $
--             newCreateRestApi
--
--         , requestCreateStage $
--             newCreateStage
--
--         , requestCreateUsagePlan $
--             newCreateUsagePlan
--
--         , requestCreateUsagePlanKey $
--             newCreateUsagePlanKey
--
--         , requestCreateVpcLink $
--             newCreateVpcLink
--
--         , requestDeleteApiKey $
--             newDeleteApiKey
--
--         , requestDeleteAuthorizer $
--             newDeleteAuthorizer
--
--         , requestDeleteBasePathMapping $
--             newDeleteBasePathMapping
--
--         , requestDeleteClientCertificate $
--             newDeleteClientCertificate
--
--         , requestDeleteDeployment $
--             newDeleteDeployment
--
--         , requestDeleteDocumentationPart $
--             newDeleteDocumentationPart
--
--         , requestDeleteDocumentationVersion $
--             newDeleteDocumentationVersion
--
--         , requestDeleteDomainName $
--             newDeleteDomainName
--
--         , requestDeleteGatewayResponse $
--             newDeleteGatewayResponse
--
--         , requestDeleteIntegration $
--             newDeleteIntegration
--
--         , requestDeleteIntegrationResponse $
--             newDeleteIntegrationResponse
--
--         , requestDeleteMethod $
--             newDeleteMethod
--
--         , requestDeleteMethodResponse $
--             newDeleteMethodResponse
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestDeleteRequestValidator $
--             newDeleteRequestValidator
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestDeleteRestApi $
--             newDeleteRestApi
--
--         , requestDeleteStage $
--             newDeleteStage
--
--         , requestDeleteUsagePlan $
--             newDeleteUsagePlan
--
--         , requestDeleteUsagePlanKey $
--             newDeleteUsagePlanKey
--
--         , requestDeleteVpcLink $
--             newDeleteVpcLink
--
--         , requestFlushStageAuthorizersCache $
--             newFlushStageAuthorizersCache
--
--         , requestFlushStageCache $
--             newFlushStageCache
--
--         , requestGenerateClientCertificate $
--             newGenerateClientCertificate
--
--         , requestGetAccount $
--             newGetAccount
--
--         , requestGetApiKey $
--             newGetApiKey
--
--         , requestGetApiKeys $
--             newGetApiKeys
--
--         , requestGetAuthorizer $
--             newGetAuthorizer
--
--         , requestGetAuthorizers $
--             newGetAuthorizers
--
--         , requestGetBasePathMapping $
--             newGetBasePathMapping
--
--         , requestGetBasePathMappings $
--             newGetBasePathMappings
--
--         , requestGetClientCertificate $
--             newGetClientCertificate
--
--         , requestGetClientCertificates $
--             newGetClientCertificates
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestGetDeployments $
--             newGetDeployments
--
--         , requestGetDocumentationPart $
--             newGetDocumentationPart
--
--         , requestGetDocumentationParts $
--             newGetDocumentationParts
--
--         , requestGetDocumentationVersion $
--             newGetDocumentationVersion
--
--         , requestGetDocumentationVersions $
--             newGetDocumentationVersions
--
--         , requestGetDomainName $
--             newGetDomainName
--
--         , requestGetDomainNames $
--             newGetDomainNames
--
--         , requestGetExport $
--             newGetExport
--
--         , requestGetGatewayResponse $
--             newGetGatewayResponse
--
--         , requestGetGatewayResponses $
--             newGetGatewayResponses
--
--         , requestGetIntegration $
--             newGetIntegration
--
--         , requestGetIntegrationResponse $
--             newGetIntegrationResponse
--
--         , requestGetMethod $
--             newGetMethod
--
--         , requestGetMethodResponse $
--             newGetMethodResponse
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
--         , requestGetRequestValidator $
--             newGetRequestValidator
--
--         , requestGetRequestValidators $
--             newGetRequestValidators
--
--         , requestGetResource $
--             newGetResource
--
--         , requestGetResources $
--             newGetResources
--
--         , requestGetRestApi $
--             newGetRestApi
--
--         , requestGetRestApis $
--             newGetRestApis
--
--         , requestGetSdk $
--             newGetSdk
--
--         , requestGetSdkType $
--             newGetSdkType
--
--         , requestGetSdkTypes $
--             newGetSdkTypes
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
--         , requestGetUsage $
--             newGetUsage
--
--         , requestGetUsagePlan $
--             newGetUsagePlan
--
--         , requestGetUsagePlanKey $
--             newGetUsagePlanKey
--
--         , requestGetUsagePlanKeys $
--             newGetUsagePlanKeys
--
--         , requestGetUsagePlans $
--             newGetUsagePlans
--
--         , requestGetVpcLink $
--             newGetVpcLink
--
--         , requestGetVpcLinks $
--             newGetVpcLinks
--
--         , requestImportApiKeys $
--             newImportApiKeys
--
--         , requestImportDocumentationParts $
--             newImportDocumentationParts
--
--         , requestImportRestApi $
--             newImportRestApi
--
--         , requestPutGatewayResponse $
--             newPutGatewayResponse
--
--         , requestPutIntegration $
--             newPutIntegration
--
--         , requestPutIntegrationResponse $
--             newPutIntegrationResponse
--
--         , requestPutMethod $
--             newPutMethod
--
--         , requestPutMethodResponse $
--             newPutMethodResponse
--
--         , requestPutRestApi $
--             newPutRestApi
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestInvokeAuthorizer $
--             newTestInvokeAuthorizer
--
--         , requestTestInvokeMethod $
--             newTestInvokeMethod
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAccount $
--             newUpdateAccount
--
--         , requestUpdateApiKey $
--             newUpdateApiKey
--
--         , requestUpdateAuthorizer $
--             newUpdateAuthorizer
--
--         , requestUpdateBasePathMapping $
--             newUpdateBasePathMapping
--
--         , requestUpdateClientCertificate $
--             newUpdateClientCertificate
--
--         , requestUpdateDeployment $
--             newUpdateDeployment
--
--         , requestUpdateDocumentationPart $
--             newUpdateDocumentationPart
--
--         , requestUpdateDocumentationVersion $
--             newUpdateDocumentationVersion
--
--         , requestUpdateDomainName $
--             newUpdateDomainName
--
--         , requestUpdateGatewayResponse $
--             newUpdateGatewayResponse
--
--         , requestUpdateIntegration $
--             newUpdateIntegration
--
--         , requestUpdateIntegrationResponse $
--             newUpdateIntegrationResponse
--
--         , requestUpdateMethod $
--             newUpdateMethod
--
--         , requestUpdateMethodResponse $
--             newUpdateMethodResponse
--
--         , requestUpdateModel $
--             newUpdateModel
--
--         , requestUpdateRequestValidator $
--             newUpdateRequestValidator
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestUpdateRestApi $
--             newUpdateRestApi
--
--         , requestUpdateStage $
--             newUpdateStage
--
--         , requestUpdateUsage $
--             newUpdateUsage
--
--         , requestUpdateUsagePlan $
--             newUpdateUsagePlan
--
--         , requestUpdateVpcLink $
--             newUpdateVpcLink
--
--           ]

--     , testGroup "response"
--         [ responseCreateApiKey $
--             newApiKey
--
--         , responseCreateAuthorizer $
--             newAuthorizer
--
--         , responseCreateBasePathMapping $
--             newBasePathMapping
--
--         , responseCreateDeployment $
--             newDeployment
--
--         , responseCreateDocumentationPart $
--             newDocumentationPart
--
--         , responseCreateDocumentationVersion $
--             newDocumentationVersion
--
--         , responseCreateDomainName $
--             newDomainName
--
--         , responseCreateModel $
--             newModel
--
--         , responseCreateRequestValidator $
--             newRequestValidator
--
--         , responseCreateResource $
--             newResource
--
--         , responseCreateRestApi $
--             newRestApi
--
--         , responseCreateStage $
--             newStage
--
--         , responseCreateUsagePlan $
--             newUsagePlan
--
--         , responseCreateUsagePlanKey $
--             newUsagePlanKey
--
--         , responseCreateVpcLink $
--             newVpcLink
--
--         , responseDeleteApiKey $
--             newDeleteApiKeyResponse
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseDeleteBasePathMapping $
--             newDeleteBasePathMappingResponse
--
--         , responseDeleteClientCertificate $
--             newDeleteClientCertificateResponse
--
--         , responseDeleteDeployment $
--             newDeleteDeploymentResponse
--
--         , responseDeleteDocumentationPart $
--             newDeleteDocumentationPartResponse
--
--         , responseDeleteDocumentationVersion $
--             newDeleteDocumentationVersionResponse
--
--         , responseDeleteDomainName $
--             newDeleteDomainNameResponse
--
--         , responseDeleteGatewayResponse $
--             newDeleteGatewayResponseResponse
--
--         , responseDeleteIntegration $
--             newDeleteIntegrationResponse'
--
--         , responseDeleteIntegrationResponse $
--             newDeleteIntegrationResponseResponse
--
--         , responseDeleteMethod $
--             newDeleteMethodResponse'
--
--         , responseDeleteMethodResponse $
--             newDeleteMethodResponseResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseDeleteRequestValidator $
--             newDeleteRequestValidatorResponse
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseDeleteRestApi $
--             newDeleteRestApiResponse
--
--         , responseDeleteStage $
--             newDeleteStageResponse
--
--         , responseDeleteUsagePlan $
--             newDeleteUsagePlanResponse
--
--         , responseDeleteUsagePlanKey $
--             newDeleteUsagePlanKeyResponse
--
--         , responseDeleteVpcLink $
--             newDeleteVpcLinkResponse
--
--         , responseFlushStageAuthorizersCache $
--             newFlushStageAuthorizersCacheResponse
--
--         , responseFlushStageCache $
--             newFlushStageCacheResponse
--
--         , responseGenerateClientCertificate $
--             newClientCertificate
--
--         , responseGetAccount $
--             newAccount
--
--         , responseGetApiKey $
--             newApiKey
--
--         , responseGetApiKeys $
--             newGetApiKeysResponse
--
--         , responseGetAuthorizer $
--             newAuthorizer
--
--         , responseGetAuthorizers $
--             newGetAuthorizersResponse
--
--         , responseGetBasePathMapping $
--             newBasePathMapping
--
--         , responseGetBasePathMappings $
--             newGetBasePathMappingsResponse
--
--         , responseGetClientCertificate $
--             newClientCertificate
--
--         , responseGetClientCertificates $
--             newGetClientCertificatesResponse
--
--         , responseGetDeployment $
--             newDeployment
--
--         , responseGetDeployments $
--             newGetDeploymentsResponse
--
--         , responseGetDocumentationPart $
--             newDocumentationPart
--
--         , responseGetDocumentationParts $
--             newGetDocumentationPartsResponse
--
--         , responseGetDocumentationVersion $
--             newDocumentationVersion
--
--         , responseGetDocumentationVersions $
--             newGetDocumentationVersionsResponse
--
--         , responseGetDomainName $
--             newDomainName
--
--         , responseGetDomainNames $
--             newGetDomainNamesResponse
--
--         , responseGetExport $
--             newGetExportResponse
--
--         , responseGetGatewayResponse $
--             newGatewayResponse
--
--         , responseGetGatewayResponses $
--             newGetGatewayResponsesResponse
--
--         , responseGetIntegration $
--             newIntegration
--
--         , responseGetIntegrationResponse $
--             newIntegrationResponse
--
--         , responseGetMethod $
--             newMethod
--
--         , responseGetMethodResponse $
--             newMethodResponse
--
--         , responseGetModel $
--             newModel
--
--         , responseGetModelTemplate $
--             newGetModelTemplateResponse
--
--         , responseGetModels $
--             newGetModelsResponse
--
--         , responseGetRequestValidator $
--             newRequestValidator
--
--         , responseGetRequestValidators $
--             newGetRequestValidatorsResponse
--
--         , responseGetResource $
--             newResource
--
--         , responseGetResources $
--             newGetResourcesResponse
--
--         , responseGetRestApi $
--             newRestApi
--
--         , responseGetRestApis $
--             newGetRestApisResponse
--
--         , responseGetSdk $
--             newGetSdkResponse
--
--         , responseGetSdkType $
--             newSdkType
--
--         , responseGetSdkTypes $
--             newGetSdkTypesResponse
--
--         , responseGetStage $
--             newStage
--
--         , responseGetStages $
--             newGetStagesResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseGetUsage $
--             newUsage
--
--         , responseGetUsagePlan $
--             newUsagePlan
--
--         , responseGetUsagePlanKey $
--             newUsagePlanKey
--
--         , responseGetUsagePlanKeys $
--             newGetUsagePlanKeysResponse
--
--         , responseGetUsagePlans $
--             newGetUsagePlansResponse
--
--         , responseGetVpcLink $
--             newVpcLink
--
--         , responseGetVpcLinks $
--             newGetVpcLinksResponse
--
--         , responseImportApiKeys $
--             newImportApiKeysResponse
--
--         , responseImportDocumentationParts $
--             newImportDocumentationPartsResponse
--
--         , responseImportRestApi $
--             newRestApi
--
--         , responsePutGatewayResponse $
--             newGatewayResponse
--
--         , responsePutIntegration $
--             newIntegration
--
--         , responsePutIntegrationResponse $
--             newIntegrationResponse
--
--         , responsePutMethod $
--             newMethod
--
--         , responsePutMethodResponse $
--             newMethodResponse
--
--         , responsePutRestApi $
--             newRestApi
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestInvokeAuthorizer $
--             newTestInvokeAuthorizerResponse
--
--         , responseTestInvokeMethod $
--             newTestInvokeMethodResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAccount $
--             newAccount
--
--         , responseUpdateApiKey $
--             newApiKey
--
--         , responseUpdateAuthorizer $
--             newAuthorizer
--
--         , responseUpdateBasePathMapping $
--             newBasePathMapping
--
--         , responseUpdateClientCertificate $
--             newClientCertificate
--
--         , responseUpdateDeployment $
--             newDeployment
--
--         , responseUpdateDocumentationPart $
--             newDocumentationPart
--
--         , responseUpdateDocumentationVersion $
--             newDocumentationVersion
--
--         , responseUpdateDomainName $
--             newDomainName
--
--         , responseUpdateGatewayResponse $
--             newGatewayResponse
--
--         , responseUpdateIntegration $
--             newIntegration
--
--         , responseUpdateIntegrationResponse $
--             newIntegrationResponse
--
--         , responseUpdateMethod $
--             newMethod
--
--         , responseUpdateMethodResponse $
--             newMethodResponse
--
--         , responseUpdateModel $
--             newModel
--
--         , responseUpdateRequestValidator $
--             newRequestValidator
--
--         , responseUpdateResource $
--             newResource
--
--         , responseUpdateRestApi $
--             newRestApi
--
--         , responseUpdateStage $
--             newStage
--
--         , responseUpdateUsage $
--             newUsage
--
--         , responseUpdateUsagePlan $
--             newUsagePlan
--
--         , responseUpdateVpcLink $
--             newVpcLink
--
--           ]
--     ]

-- Requests

requestCreateApiKey :: CreateApiKey -> TestTree
requestCreateApiKey =
  req
    "CreateApiKey"
    "fixture/CreateApiKey.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer =
  req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestCreateBasePathMapping :: CreateBasePathMapping -> TestTree
requestCreateBasePathMapping =
  req
    "CreateBasePathMapping"
    "fixture/CreateBasePathMapping.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestCreateDocumentationPart :: CreateDocumentationPart -> TestTree
requestCreateDocumentationPart =
  req
    "CreateDocumentationPart"
    "fixture/CreateDocumentationPart.yaml"

requestCreateDocumentationVersion :: CreateDocumentationVersion -> TestTree
requestCreateDocumentationVersion =
  req
    "CreateDocumentationVersion"
    "fixture/CreateDocumentationVersion.yaml"

requestCreateDomainName :: CreateDomainName -> TestTree
requestCreateDomainName =
  req
    "CreateDomainName"
    "fixture/CreateDomainName.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestCreateRequestValidator :: CreateRequestValidator -> TestTree
requestCreateRequestValidator =
  req
    "CreateRequestValidator"
    "fixture/CreateRequestValidator.yaml"

requestCreateResource :: CreateResource -> TestTree
requestCreateResource =
  req
    "CreateResource"
    "fixture/CreateResource.yaml"

requestCreateRestApi :: CreateRestApi -> TestTree
requestCreateRestApi =
  req
    "CreateRestApi"
    "fixture/CreateRestApi.yaml"

requestCreateStage :: CreateStage -> TestTree
requestCreateStage =
  req
    "CreateStage"
    "fixture/CreateStage.yaml"

requestCreateUsagePlan :: CreateUsagePlan -> TestTree
requestCreateUsagePlan =
  req
    "CreateUsagePlan"
    "fixture/CreateUsagePlan.yaml"

requestCreateUsagePlanKey :: CreateUsagePlanKey -> TestTree
requestCreateUsagePlanKey =
  req
    "CreateUsagePlanKey"
    "fixture/CreateUsagePlanKey.yaml"

requestCreateVpcLink :: CreateVpcLink -> TestTree
requestCreateVpcLink =
  req
    "CreateVpcLink"
    "fixture/CreateVpcLink.yaml"

requestDeleteApiKey :: DeleteApiKey -> TestTree
requestDeleteApiKey =
  req
    "DeleteApiKey"
    "fixture/DeleteApiKey.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer =
  req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestDeleteBasePathMapping :: DeleteBasePathMapping -> TestTree
requestDeleteBasePathMapping =
  req
    "DeleteBasePathMapping"
    "fixture/DeleteBasePathMapping.yaml"

requestDeleteClientCertificate :: DeleteClientCertificate -> TestTree
requestDeleteClientCertificate =
  req
    "DeleteClientCertificate"
    "fixture/DeleteClientCertificate.yaml"

requestDeleteDeployment :: DeleteDeployment -> TestTree
requestDeleteDeployment =
  req
    "DeleteDeployment"
    "fixture/DeleteDeployment.yaml"

requestDeleteDocumentationPart :: DeleteDocumentationPart -> TestTree
requestDeleteDocumentationPart =
  req
    "DeleteDocumentationPart"
    "fixture/DeleteDocumentationPart.yaml"

requestDeleteDocumentationVersion :: DeleteDocumentationVersion -> TestTree
requestDeleteDocumentationVersion =
  req
    "DeleteDocumentationVersion"
    "fixture/DeleteDocumentationVersion.yaml"

requestDeleteDomainName :: DeleteDomainName -> TestTree
requestDeleteDomainName =
  req
    "DeleteDomainName"
    "fixture/DeleteDomainName.yaml"

requestDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
requestDeleteGatewayResponse =
  req
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.yaml"

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

requestDeleteMethod :: DeleteMethod -> TestTree
requestDeleteMethod =
  req
    "DeleteMethod"
    "fixture/DeleteMethod.yaml"

requestDeleteMethodResponse :: DeleteMethodResponse -> TestTree
requestDeleteMethodResponse =
  req
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestDeleteRequestValidator :: DeleteRequestValidator -> TestTree
requestDeleteRequestValidator =
  req
    "DeleteRequestValidator"
    "fixture/DeleteRequestValidator.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestDeleteRestApi :: DeleteRestApi -> TestTree
requestDeleteRestApi =
  req
    "DeleteRestApi"
    "fixture/DeleteRestApi.yaml"

requestDeleteStage :: DeleteStage -> TestTree
requestDeleteStage =
  req
    "DeleteStage"
    "fixture/DeleteStage.yaml"

requestDeleteUsagePlan :: DeleteUsagePlan -> TestTree
requestDeleteUsagePlan =
  req
    "DeleteUsagePlan"
    "fixture/DeleteUsagePlan.yaml"

requestDeleteUsagePlanKey :: DeleteUsagePlanKey -> TestTree
requestDeleteUsagePlanKey =
  req
    "DeleteUsagePlanKey"
    "fixture/DeleteUsagePlanKey.yaml"

requestDeleteVpcLink :: DeleteVpcLink -> TestTree
requestDeleteVpcLink =
  req
    "DeleteVpcLink"
    "fixture/DeleteVpcLink.yaml"

requestFlushStageAuthorizersCache :: FlushStageAuthorizersCache -> TestTree
requestFlushStageAuthorizersCache =
  req
    "FlushStageAuthorizersCache"
    "fixture/FlushStageAuthorizersCache.yaml"

requestFlushStageCache :: FlushStageCache -> TestTree
requestFlushStageCache =
  req
    "FlushStageCache"
    "fixture/FlushStageCache.yaml"

requestGenerateClientCertificate :: GenerateClientCertificate -> TestTree
requestGenerateClientCertificate =
  req
    "GenerateClientCertificate"
    "fixture/GenerateClientCertificate.yaml"

requestGetAccount :: GetAccount -> TestTree
requestGetAccount =
  req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestGetApiKey :: GetApiKey -> TestTree
requestGetApiKey =
  req
    "GetApiKey"
    "fixture/GetApiKey.yaml"

requestGetApiKeys :: GetApiKeys -> TestTree
requestGetApiKeys =
  req
    "GetApiKeys"
    "fixture/GetApiKeys.yaml"

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

requestGetBasePathMapping :: GetBasePathMapping -> TestTree
requestGetBasePathMapping =
  req
    "GetBasePathMapping"
    "fixture/GetBasePathMapping.yaml"

requestGetBasePathMappings :: GetBasePathMappings -> TestTree
requestGetBasePathMappings =
  req
    "GetBasePathMappings"
    "fixture/GetBasePathMappings.yaml"

requestGetClientCertificate :: GetClientCertificate -> TestTree
requestGetClientCertificate =
  req
    "GetClientCertificate"
    "fixture/GetClientCertificate.yaml"

requestGetClientCertificates :: GetClientCertificates -> TestTree
requestGetClientCertificates =
  req
    "GetClientCertificates"
    "fixture/GetClientCertificates.yaml"

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

requestGetDocumentationPart :: GetDocumentationPart -> TestTree
requestGetDocumentationPart =
  req
    "GetDocumentationPart"
    "fixture/GetDocumentationPart.yaml"

requestGetDocumentationParts :: GetDocumentationParts -> TestTree
requestGetDocumentationParts =
  req
    "GetDocumentationParts"
    "fixture/GetDocumentationParts.yaml"

requestGetDocumentationVersion :: GetDocumentationVersion -> TestTree
requestGetDocumentationVersion =
  req
    "GetDocumentationVersion"
    "fixture/GetDocumentationVersion.yaml"

requestGetDocumentationVersions :: GetDocumentationVersions -> TestTree
requestGetDocumentationVersions =
  req
    "GetDocumentationVersions"
    "fixture/GetDocumentationVersions.yaml"

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

requestGetExport :: GetExport -> TestTree
requestGetExport =
  req
    "GetExport"
    "fixture/GetExport.yaml"

requestGetGatewayResponse :: GetGatewayResponse -> TestTree
requestGetGatewayResponse =
  req
    "GetGatewayResponse"
    "fixture/GetGatewayResponse.yaml"

requestGetGatewayResponses :: GetGatewayResponses -> TestTree
requestGetGatewayResponses =
  req
    "GetGatewayResponses"
    "fixture/GetGatewayResponses.yaml"

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

requestGetMethod :: GetMethod -> TestTree
requestGetMethod =
  req
    "GetMethod"
    "fixture/GetMethod.yaml"

requestGetMethodResponse :: GetMethodResponse -> TestTree
requestGetMethodResponse =
  req
    "GetMethodResponse"
    "fixture/GetMethodResponse.yaml"

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

requestGetRequestValidator :: GetRequestValidator -> TestTree
requestGetRequestValidator =
  req
    "GetRequestValidator"
    "fixture/GetRequestValidator.yaml"

requestGetRequestValidators :: GetRequestValidators -> TestTree
requestGetRequestValidators =
  req
    "GetRequestValidators"
    "fixture/GetRequestValidators.yaml"

requestGetResource :: GetResource -> TestTree
requestGetResource =
  req
    "GetResource"
    "fixture/GetResource.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources =
  req
    "GetResources"
    "fixture/GetResources.yaml"

requestGetRestApi :: GetRestApi -> TestTree
requestGetRestApi =
  req
    "GetRestApi"
    "fixture/GetRestApi.yaml"

requestGetRestApis :: GetRestApis -> TestTree
requestGetRestApis =
  req
    "GetRestApis"
    "fixture/GetRestApis.yaml"

requestGetSdk :: GetSdk -> TestTree
requestGetSdk =
  req
    "GetSdk"
    "fixture/GetSdk.yaml"

requestGetSdkType :: GetSdkType -> TestTree
requestGetSdkType =
  req
    "GetSdkType"
    "fixture/GetSdkType.yaml"

requestGetSdkTypes :: GetSdkTypes -> TestTree
requestGetSdkTypes =
  req
    "GetSdkTypes"
    "fixture/GetSdkTypes.yaml"

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

requestGetUsage :: GetUsage -> TestTree
requestGetUsage =
  req
    "GetUsage"
    "fixture/GetUsage.yaml"

requestGetUsagePlan :: GetUsagePlan -> TestTree
requestGetUsagePlan =
  req
    "GetUsagePlan"
    "fixture/GetUsagePlan.yaml"

requestGetUsagePlanKey :: GetUsagePlanKey -> TestTree
requestGetUsagePlanKey =
  req
    "GetUsagePlanKey"
    "fixture/GetUsagePlanKey.yaml"

requestGetUsagePlanKeys :: GetUsagePlanKeys -> TestTree
requestGetUsagePlanKeys =
  req
    "GetUsagePlanKeys"
    "fixture/GetUsagePlanKeys.yaml"

requestGetUsagePlans :: GetUsagePlans -> TestTree
requestGetUsagePlans =
  req
    "GetUsagePlans"
    "fixture/GetUsagePlans.yaml"

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

requestImportApiKeys :: ImportApiKeys -> TestTree
requestImportApiKeys =
  req
    "ImportApiKeys"
    "fixture/ImportApiKeys.yaml"

requestImportDocumentationParts :: ImportDocumentationParts -> TestTree
requestImportDocumentationParts =
  req
    "ImportDocumentationParts"
    "fixture/ImportDocumentationParts.yaml"

requestImportRestApi :: ImportRestApi -> TestTree
requestImportRestApi =
  req
    "ImportRestApi"
    "fixture/ImportRestApi.yaml"

requestPutGatewayResponse :: PutGatewayResponse -> TestTree
requestPutGatewayResponse =
  req
    "PutGatewayResponse"
    "fixture/PutGatewayResponse.yaml"

requestPutIntegration :: PutIntegration -> TestTree
requestPutIntegration =
  req
    "PutIntegration"
    "fixture/PutIntegration.yaml"

requestPutIntegrationResponse :: PutIntegrationResponse -> TestTree
requestPutIntegrationResponse =
  req
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.yaml"

requestPutMethod :: PutMethod -> TestTree
requestPutMethod =
  req
    "PutMethod"
    "fixture/PutMethod.yaml"

requestPutMethodResponse :: PutMethodResponse -> TestTree
requestPutMethodResponse =
  req
    "PutMethodResponse"
    "fixture/PutMethodResponse.yaml"

requestPutRestApi :: PutRestApi -> TestTree
requestPutRestApi =
  req
    "PutRestApi"
    "fixture/PutRestApi.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer =
  req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

requestTestInvokeMethod :: TestInvokeMethod -> TestTree
requestTestInvokeMethod =
  req
    "TestInvokeMethod"
    "fixture/TestInvokeMethod.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAccount :: UpdateAccount -> TestTree
requestUpdateAccount =
  req
    "UpdateAccount"
    "fixture/UpdateAccount.yaml"

requestUpdateApiKey :: UpdateApiKey -> TestTree
requestUpdateApiKey =
  req
    "UpdateApiKey"
    "fixture/UpdateApiKey.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer =
  req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestUpdateBasePathMapping :: UpdateBasePathMapping -> TestTree
requestUpdateBasePathMapping =
  req
    "UpdateBasePathMapping"
    "fixture/UpdateBasePathMapping.yaml"

requestUpdateClientCertificate :: UpdateClientCertificate -> TestTree
requestUpdateClientCertificate =
  req
    "UpdateClientCertificate"
    "fixture/UpdateClientCertificate.yaml"

requestUpdateDeployment :: UpdateDeployment -> TestTree
requestUpdateDeployment =
  req
    "UpdateDeployment"
    "fixture/UpdateDeployment.yaml"

requestUpdateDocumentationPart :: UpdateDocumentationPart -> TestTree
requestUpdateDocumentationPart =
  req
    "UpdateDocumentationPart"
    "fixture/UpdateDocumentationPart.yaml"

requestUpdateDocumentationVersion :: UpdateDocumentationVersion -> TestTree
requestUpdateDocumentationVersion =
  req
    "UpdateDocumentationVersion"
    "fixture/UpdateDocumentationVersion.yaml"

requestUpdateDomainName :: UpdateDomainName -> TestTree
requestUpdateDomainName =
  req
    "UpdateDomainName"
    "fixture/UpdateDomainName.yaml"

requestUpdateGatewayResponse :: UpdateGatewayResponse -> TestTree
requestUpdateGatewayResponse =
  req
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.yaml"

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

requestUpdateMethod :: UpdateMethod -> TestTree
requestUpdateMethod =
  req
    "UpdateMethod"
    "fixture/UpdateMethod.yaml"

requestUpdateMethodResponse :: UpdateMethodResponse -> TestTree
requestUpdateMethodResponse =
  req
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.yaml"

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel =
  req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestUpdateRequestValidator :: UpdateRequestValidator -> TestTree
requestUpdateRequestValidator =
  req
    "UpdateRequestValidator"
    "fixture/UpdateRequestValidator.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestUpdateRestApi :: UpdateRestApi -> TestTree
requestUpdateRestApi =
  req
    "UpdateRestApi"
    "fixture/UpdateRestApi.yaml"

requestUpdateStage :: UpdateStage -> TestTree
requestUpdateStage =
  req
    "UpdateStage"
    "fixture/UpdateStage.yaml"

requestUpdateUsage :: UpdateUsage -> TestTree
requestUpdateUsage =
  req
    "UpdateUsage"
    "fixture/UpdateUsage.yaml"

requestUpdateUsagePlan :: UpdateUsagePlan -> TestTree
requestUpdateUsagePlan =
  req
    "UpdateUsagePlan"
    "fixture/UpdateUsagePlan.yaml"

requestUpdateVpcLink :: UpdateVpcLink -> TestTree
requestUpdateVpcLink =
  req
    "UpdateVpcLink"
    "fixture/UpdateVpcLink.yaml"

-- Responses

responseCreateApiKey :: ApiKey -> TestTree
responseCreateApiKey =
  res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiKey)

responseCreateAuthorizer :: Authorizer -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuthorizer)

responseCreateBasePathMapping :: BasePathMapping -> TestTree
responseCreateBasePathMapping =
  res
    "CreateBasePathMappingResponse"
    "fixture/CreateBasePathMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBasePathMapping)

responseCreateDeployment :: Deployment -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseCreateDocumentationPart :: DocumentationPart -> TestTree
responseCreateDocumentationPart =
  res
    "CreateDocumentationPartResponse"
    "fixture/CreateDocumentationPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDocumentationPart)

responseCreateDocumentationVersion :: DocumentationVersion -> TestTree
responseCreateDocumentationVersion =
  res
    "CreateDocumentationVersionResponse"
    "fixture/CreateDocumentationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDocumentationVersion)

responseCreateDomainName :: DomainName -> TestTree
responseCreateDomainName =
  res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainName)

responseCreateModel :: Model -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

responseCreateRequestValidator :: RequestValidator -> TestTree
responseCreateRequestValidator =
  res
    "CreateRequestValidatorResponse"
    "fixture/CreateRequestValidatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRequestValidator)

responseCreateResource :: Resource -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResource)

responseCreateRestApi :: RestApi -> TestTree
responseCreateRestApi =
  res
    "CreateRestApiResponse"
    "fixture/CreateRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRestApi)

responseCreateStage :: Stage -> TestTree
responseCreateStage =
  res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStage)

responseCreateUsagePlan :: UsagePlan -> TestTree
responseCreateUsagePlan =
  res
    "CreateUsagePlanResponse"
    "fixture/CreateUsagePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUsagePlan)

responseCreateUsagePlanKey :: UsagePlanKey -> TestTree
responseCreateUsagePlanKey =
  res
    "CreateUsagePlanKeyResponse"
    "fixture/CreateUsagePlanKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUsagePlanKey)

responseCreateVpcLink :: VpcLink -> TestTree
responseCreateVpcLink =
  res
    "CreateVpcLinkResponse"
    "fixture/CreateVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcLink)

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey =
  res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiKey)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAuthorizer)

responseDeleteBasePathMapping :: DeleteBasePathMappingResponse -> TestTree
responseDeleteBasePathMapping =
  res
    "DeleteBasePathMappingResponse"
    "fixture/DeleteBasePathMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBasePathMapping)

responseDeleteClientCertificate :: DeleteClientCertificateResponse -> TestTree
responseDeleteClientCertificate =
  res
    "DeleteClientCertificateResponse"
    "fixture/DeleteClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClientCertificate)

responseDeleteDeployment :: DeleteDeploymentResponse -> TestTree
responseDeleteDeployment =
  res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeployment)

responseDeleteDocumentationPart :: DeleteDocumentationPartResponse -> TestTree
responseDeleteDocumentationPart =
  res
    "DeleteDocumentationPartResponse"
    "fixture/DeleteDocumentationPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocumentationPart)

responseDeleteDocumentationVersion :: DeleteDocumentationVersionResponse -> TestTree
responseDeleteDocumentationVersion =
  res
    "DeleteDocumentationVersionResponse"
    "fixture/DeleteDocumentationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocumentationVersion)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName =
  res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainName)

responseDeleteGatewayResponse :: DeleteGatewayResponseResponse -> TestTree
responseDeleteGatewayResponse =
  res
    "DeleteGatewayResponseResponse"
    "fixture/DeleteGatewayResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGatewayResponse)

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

responseDeleteMethod :: DeleteMethodResponse' -> TestTree
responseDeleteMethod =
  res
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMethod)

responseDeleteMethodResponse :: DeleteMethodResponseResponse -> TestTree
responseDeleteMethodResponse =
  res
    "DeleteMethodResponseResponse"
    "fixture/DeleteMethodResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMethodResponse)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseDeleteRequestValidator :: DeleteRequestValidatorResponse -> TestTree
responseDeleteRequestValidator =
  res
    "DeleteRequestValidatorResponse"
    "fixture/DeleteRequestValidatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRequestValidator)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResource)

responseDeleteRestApi :: DeleteRestApiResponse -> TestTree
responseDeleteRestApi =
  res
    "DeleteRestApiResponse"
    "fixture/DeleteRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRestApi)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage =
  res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStage)

responseDeleteUsagePlan :: DeleteUsagePlanResponse -> TestTree
responseDeleteUsagePlan =
  res
    "DeleteUsagePlanResponse"
    "fixture/DeleteUsagePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUsagePlan)

responseDeleteUsagePlanKey :: DeleteUsagePlanKeyResponse -> TestTree
responseDeleteUsagePlanKey =
  res
    "DeleteUsagePlanKeyResponse"
    "fixture/DeleteUsagePlanKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUsagePlanKey)

responseDeleteVpcLink :: DeleteVpcLinkResponse -> TestTree
responseDeleteVpcLink =
  res
    "DeleteVpcLinkResponse"
    "fixture/DeleteVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcLink)

responseFlushStageAuthorizersCache :: FlushStageAuthorizersCacheResponse -> TestTree
responseFlushStageAuthorizersCache =
  res
    "FlushStageAuthorizersCacheResponse"
    "fixture/FlushStageAuthorizersCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FlushStageAuthorizersCache)

responseFlushStageCache :: FlushStageCacheResponse -> TestTree
responseFlushStageCache =
  res
    "FlushStageCacheResponse"
    "fixture/FlushStageCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FlushStageCache)

responseGenerateClientCertificate :: ClientCertificate -> TestTree
responseGenerateClientCertificate =
  res
    "GenerateClientCertificateResponse"
    "fixture/GenerateClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateClientCertificate)

responseGetAccount :: Account -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccount)

responseGetApiKey :: ApiKey -> TestTree
responseGetApiKey =
  res
    "GetApiKeyResponse"
    "fixture/GetApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiKey)

responseGetApiKeys :: GetApiKeysResponse -> TestTree
responseGetApiKeys =
  res
    "GetApiKeysResponse"
    "fixture/GetApiKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiKeys)

responseGetAuthorizer :: Authorizer -> TestTree
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

responseGetBasePathMapping :: BasePathMapping -> TestTree
responseGetBasePathMapping =
  res
    "GetBasePathMappingResponse"
    "fixture/GetBasePathMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBasePathMapping)

responseGetBasePathMappings :: GetBasePathMappingsResponse -> TestTree
responseGetBasePathMappings =
  res
    "GetBasePathMappingsResponse"
    "fixture/GetBasePathMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBasePathMappings)

responseGetClientCertificate :: ClientCertificate -> TestTree
responseGetClientCertificate =
  res
    "GetClientCertificateResponse"
    "fixture/GetClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClientCertificate)

responseGetClientCertificates :: GetClientCertificatesResponse -> TestTree
responseGetClientCertificates =
  res
    "GetClientCertificatesResponse"
    "fixture/GetClientCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClientCertificates)

responseGetDeployment :: Deployment -> TestTree
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

responseGetDocumentationPart :: DocumentationPart -> TestTree
responseGetDocumentationPart =
  res
    "GetDocumentationPartResponse"
    "fixture/GetDocumentationPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentationPart)

responseGetDocumentationParts :: GetDocumentationPartsResponse -> TestTree
responseGetDocumentationParts =
  res
    "GetDocumentationPartsResponse"
    "fixture/GetDocumentationPartsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentationParts)

responseGetDocumentationVersion :: DocumentationVersion -> TestTree
responseGetDocumentationVersion =
  res
    "GetDocumentationVersionResponse"
    "fixture/GetDocumentationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentationVersion)

responseGetDocumentationVersions :: GetDocumentationVersionsResponse -> TestTree
responseGetDocumentationVersions =
  res
    "GetDocumentationVersionsResponse"
    "fixture/GetDocumentationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentationVersions)

responseGetDomainName :: DomainName -> TestTree
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

responseGetExport :: GetExportResponse -> TestTree
responseGetExport =
  res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExport)

responseGetGatewayResponse :: GatewayResponse -> TestTree
responseGetGatewayResponse =
  res
    "GetGatewayResponseResponse"
    "fixture/GetGatewayResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGatewayResponse)

responseGetGatewayResponses :: GetGatewayResponsesResponse -> TestTree
responseGetGatewayResponses =
  res
    "GetGatewayResponsesResponse"
    "fixture/GetGatewayResponsesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGatewayResponses)

responseGetIntegration :: Integration -> TestTree
responseGetIntegration =
  res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegration)

responseGetIntegrationResponse :: IntegrationResponse -> TestTree
responseGetIntegrationResponse =
  res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegrationResponse)

responseGetMethod :: Method -> TestTree
responseGetMethod =
  res
    "GetMethodResponse"
    "fixture/GetMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMethod)

responseGetMethodResponse :: MethodResponse -> TestTree
responseGetMethodResponse =
  res
    "GetMethodResponseResponse"
    "fixture/GetMethodResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMethodResponse)

responseGetModel :: Model -> TestTree
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

responseGetRequestValidator :: RequestValidator -> TestTree
responseGetRequestValidator =
  res
    "GetRequestValidatorResponse"
    "fixture/GetRequestValidatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRequestValidator)

responseGetRequestValidators :: GetRequestValidatorsResponse -> TestTree
responseGetRequestValidators =
  res
    "GetRequestValidatorsResponse"
    "fixture/GetRequestValidatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRequestValidators)

responseGetResource :: Resource -> TestTree
responseGetResource =
  res
    "GetResourceResponse"
    "fixture/GetResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResource)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResources)

responseGetRestApi :: RestApi -> TestTree
responseGetRestApi =
  res
    "GetRestApiResponse"
    "fixture/GetRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRestApi)

responseGetRestApis :: GetRestApisResponse -> TestTree
responseGetRestApis =
  res
    "GetRestApisResponse"
    "fixture/GetRestApisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRestApis)

responseGetSdk :: GetSdkResponse -> TestTree
responseGetSdk =
  res
    "GetSdkResponse"
    "fixture/GetSdkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSdk)

responseGetSdkType :: SdkType -> TestTree
responseGetSdkType =
  res
    "GetSdkTypeResponse"
    "fixture/GetSdkTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSdkType)

responseGetSdkTypes :: GetSdkTypesResponse -> TestTree
responseGetSdkTypes =
  res
    "GetSdkTypesResponse"
    "fixture/GetSdkTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSdkTypes)

responseGetStage :: Stage -> TestTree
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

responseGetUsage :: Usage -> TestTree
responseGetUsage =
  res
    "GetUsageResponse"
    "fixture/GetUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsage)

responseGetUsagePlan :: UsagePlan -> TestTree
responseGetUsagePlan =
  res
    "GetUsagePlanResponse"
    "fixture/GetUsagePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsagePlan)

responseGetUsagePlanKey :: UsagePlanKey -> TestTree
responseGetUsagePlanKey =
  res
    "GetUsagePlanKeyResponse"
    "fixture/GetUsagePlanKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsagePlanKey)

responseGetUsagePlanKeys :: GetUsagePlanKeysResponse -> TestTree
responseGetUsagePlanKeys =
  res
    "GetUsagePlanKeysResponse"
    "fixture/GetUsagePlanKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsagePlanKeys)

responseGetUsagePlans :: GetUsagePlansResponse -> TestTree
responseGetUsagePlans =
  res
    "GetUsagePlansResponse"
    "fixture/GetUsagePlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsagePlans)

responseGetVpcLink :: VpcLink -> TestTree
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

responseImportApiKeys :: ImportApiKeysResponse -> TestTree
responseImportApiKeys =
  res
    "ImportApiKeysResponse"
    "fixture/ImportApiKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportApiKeys)

responseImportDocumentationParts :: ImportDocumentationPartsResponse -> TestTree
responseImportDocumentationParts =
  res
    "ImportDocumentationPartsResponse"
    "fixture/ImportDocumentationPartsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportDocumentationParts)

responseImportRestApi :: RestApi -> TestTree
responseImportRestApi =
  res
    "ImportRestApiResponse"
    "fixture/ImportRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportRestApi)

responsePutGatewayResponse :: GatewayResponse -> TestTree
responsePutGatewayResponse =
  res
    "PutGatewayResponseResponse"
    "fixture/PutGatewayResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutGatewayResponse)

responsePutIntegration :: Integration -> TestTree
responsePutIntegration =
  res
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutIntegration)

responsePutIntegrationResponse :: IntegrationResponse -> TestTree
responsePutIntegrationResponse =
  res
    "PutIntegrationResponseResponse"
    "fixture/PutIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutIntegrationResponse)

responsePutMethod :: Method -> TestTree
responsePutMethod =
  res
    "PutMethodResponse"
    "fixture/PutMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMethod)

responsePutMethodResponse :: MethodResponse -> TestTree
responsePutMethodResponse =
  res
    "PutMethodResponseResponse"
    "fixture/PutMethodResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMethodResponse)

responsePutRestApi :: RestApi -> TestTree
responsePutRestApi =
  res
    "PutRestApiResponse"
    "fixture/PutRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRestApi)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestInvokeAuthorizer)

responseTestInvokeMethod :: TestInvokeMethodResponse -> TestTree
responseTestInvokeMethod =
  res
    "TestInvokeMethodResponse"
    "fixture/TestInvokeMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestInvokeMethod)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAccount :: Account -> TestTree
responseUpdateAccount =
  res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccount)

responseUpdateApiKey :: ApiKey -> TestTree
responseUpdateApiKey =
  res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiKey)

responseUpdateAuthorizer :: Authorizer -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuthorizer)

responseUpdateBasePathMapping :: BasePathMapping -> TestTree
responseUpdateBasePathMapping =
  res
    "UpdateBasePathMappingResponse"
    "fixture/UpdateBasePathMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBasePathMapping)

responseUpdateClientCertificate :: ClientCertificate -> TestTree
responseUpdateClientCertificate =
  res
    "UpdateClientCertificateResponse"
    "fixture/UpdateClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClientCertificate)

responseUpdateDeployment :: Deployment -> TestTree
responseUpdateDeployment =
  res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeployment)

responseUpdateDocumentationPart :: DocumentationPart -> TestTree
responseUpdateDocumentationPart =
  res
    "UpdateDocumentationPartResponse"
    "fixture/UpdateDocumentationPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentationPart)

responseUpdateDocumentationVersion :: DocumentationVersion -> TestTree
responseUpdateDocumentationVersion =
  res
    "UpdateDocumentationVersionResponse"
    "fixture/UpdateDocumentationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentationVersion)

responseUpdateDomainName :: DomainName -> TestTree
responseUpdateDomainName =
  res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainName)

responseUpdateGatewayResponse :: GatewayResponse -> TestTree
responseUpdateGatewayResponse =
  res
    "UpdateGatewayResponseResponse"
    "fixture/UpdateGatewayResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayResponse)

responseUpdateIntegration :: Integration -> TestTree
responseUpdateIntegration =
  res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIntegration)

responseUpdateIntegrationResponse :: IntegrationResponse -> TestTree
responseUpdateIntegrationResponse =
  res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIntegrationResponse)

responseUpdateMethod :: Method -> TestTree
responseUpdateMethod =
  res
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMethod)

responseUpdateMethodResponse :: MethodResponse -> TestTree
responseUpdateMethodResponse =
  res
    "UpdateMethodResponseResponse"
    "fixture/UpdateMethodResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMethodResponse)

responseUpdateModel :: Model -> TestTree
responseUpdateModel =
  res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModel)

responseUpdateRequestValidator :: RequestValidator -> TestTree
responseUpdateRequestValidator =
  res
    "UpdateRequestValidatorResponse"
    "fixture/UpdateRequestValidatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRequestValidator)

responseUpdateResource :: Resource -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResource)

responseUpdateRestApi :: RestApi -> TestTree
responseUpdateRestApi =
  res
    "UpdateRestApiResponse"
    "fixture/UpdateRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRestApi)

responseUpdateStage :: Stage -> TestTree
responseUpdateStage =
  res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStage)

responseUpdateUsage :: Usage -> TestTree
responseUpdateUsage =
  res
    "UpdateUsageResponse"
    "fixture/UpdateUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUsage)

responseUpdateUsagePlan :: UsagePlan -> TestTree
responseUpdateUsagePlan =
  res
    "UpdateUsagePlanResponse"
    "fixture/UpdateUsagePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUsagePlan)

responseUpdateVpcLink :: VpcLink -> TestTree
responseUpdateVpcLink =
  res
    "UpdateVpcLinkResponse"
    "fixture/UpdateVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVpcLink)
