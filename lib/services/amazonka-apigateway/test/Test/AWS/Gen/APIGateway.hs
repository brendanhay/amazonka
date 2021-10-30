{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.APIGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.APIGateway where

import qualified Data.Proxy as Proxy
import Network.AWS.APIGateway
import Test.AWS.APIGateway.Internal
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
--         [ requestGetResource $
--             newGetResource
--
--         , requestGetDeployments $
--             newGetDeployments
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestGetTags $
--             newGetTags
--
--         , requestDeleteGatewayResponse $
--             newDeleteGatewayResponse
--
--         , requestUpdateGatewayResponse $
--             newUpdateGatewayResponse
--
--         , requestCreateUsagePlan $
--             newCreateUsagePlan
--
--         , requestGetDomainNames $
--             newGetDomainNames
--
--         , requestGetClientCertificate $
--             newGetClientCertificate
--
--         , requestPutGatewayResponse $
--             newPutGatewayResponse
--
--         , requestGetSdkType $
--             newGetSdkType
--
--         , requestGetMethodResponse $
--             newGetMethodResponse
--
--         , requestGetModels $
--             newGetModels
--
--         , requestGetBasePathMapping $
--             newGetBasePathMapping
--
--         , requestGetRequestValidators $
--             newGetRequestValidators
--
--         , requestPutMethodResponse $
--             newPutMethodResponse
--
--         , requestImportRestApi $
--             newImportRestApi
--
--         , requestDeleteMethodResponse $
--             newDeleteMethodResponse
--
--         , requestUpdateMethodResponse $
--             newUpdateMethodResponse
--
--         , requestDeleteStage $
--             newDeleteStage
--
--         , requestUpdateStage $
--             newUpdateStage
--
--         , requestGetRestApis $
--             newGetRestApis
--
--         , requestGetDocumentationVersions $
--             newGetDocumentationVersions
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestGetVpcLinks $
--             newGetVpcLinks
--
--         , requestCreateBasePathMapping $
--             newCreateBasePathMapping
--
--         , requestGetIntegration $
--             newGetIntegration
--
--         , requestGetDocumentationParts $
--             newGetDocumentationParts
--
--         , requestUpdateAccount $
--             newUpdateAccount
--
--         , requestGetUsagePlan $
--             newGetUsagePlan
--
--         , requestDeleteDeployment $
--             newDeleteDeployment
--
--         , requestUpdateDeployment $
--             newUpdateDeployment
--
--         , requestGetDocumentationPart $
--             newGetDocumentationPart
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestCreateRequestValidator $
--             newCreateRequestValidator
--
--         , requestImportDocumentationParts $
--             newImportDocumentationParts
--
--         , requestGetUsage $
--             newGetUsage
--
--         , requestGetVpcLink $
--             newGetVpcLink
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
--         , requestFlushStageAuthorizersCache $
--             newFlushStageAuthorizersCache
--
--         , requestGetGatewayResponses $
--             newGetGatewayResponses
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestUpdateModel $
--             newUpdateModel
--
--         , requestGetDocumentationVersion $
--             newGetDocumentationVersion
--
--         , requestDeleteApiKey $
--             newDeleteApiKey
--
--         , requestUpdateApiKey $
--             newUpdateApiKey
--
--         , requestGetRestApi $
--             newGetRestApi
--
--         , requestGetStages $
--             newGetStages
--
--         , requestPutRestApi $
--             newPutRestApi
--
--         , requestGetMethod $
--             newGetMethod
--
--         , requestGetModel $
--             newGetModel
--
--         , requestUpdateRestApi $
--             newUpdateRestApi
--
--         , requestDeleteRestApi $
--             newDeleteRestApi
--
--         , requestImportApiKeys $
--             newImportApiKeys
--
--         , requestCreateDocumentationPart $
--             newCreateDocumentationPart
--
--         , requestTestInvokeMethod $
--             newTestInvokeMethod
--
--         , requestGetRequestValidator $
--             newGetRequestValidator
--
--         , requestGetDomainName $
--             newGetDomainName
--
--         , requestCreateVpcLink $
--             newCreateVpcLink
--
--         , requestDeleteDocumentationPart $
--             newDeleteDocumentationPart
--
--         , requestUpdateDocumentationPart $
--             newUpdateDocumentationPart
--
--         , requestGetAuthorizers $
--             newGetAuthorizers
--
--         , requestCreateDocumentationVersion $
--             newCreateDocumentationVersion
--
--         , requestPutIntegrationResponse $
--             newPutIntegrationResponse
--
--         , requestGetUsagePlanKeys $
--             newGetUsagePlanKeys
--
--         , requestDeleteVpcLink $
--             newDeleteVpcLink
--
--         , requestUpdateVpcLink $
--             newUpdateVpcLink
--
--         , requestFlushStageCache $
--             newFlushStageCache
--
--         , requestCreateRestApi $
--             newCreateRestApi
--
--         , requestDeleteIntegrationResponse $
--             newDeleteIntegrationResponse
--
--         , requestUpdateIntegrationResponse $
--             newUpdateIntegrationResponse
--
--         , requestUpdateUsage $
--             newUpdateUsage
--
--         , requestDeleteIntegration $
--             newDeleteIntegration
--
--         , requestUpdateIntegration $
--             newUpdateIntegration
--
--         , requestTestInvokeAuthorizer $
--             newTestInvokeAuthorizer
--
--         , requestGenerateClientCertificate $
--             newGenerateClientCertificate
--
--         , requestGetResources $
--             newGetResources
--
--         , requestGetUsagePlanKey $
--             newGetUsagePlanKey
--
--         , requestGetAccount $
--             newGetAccount
--
--         , requestPutIntegration $
--             newPutIntegration
--
--         , requestGetAuthorizer $
--             newGetAuthorizer
--
--         , requestDeleteUsagePlan $
--             newDeleteUsagePlan
--
--         , requestUpdateUsagePlan $
--             newUpdateUsagePlan
--
--         , requestGetStage $
--             newGetStage
--
--         , requestGetExport $
--             newGetExport
--
--         , requestGetSdk $
--             newGetSdk
--
--         , requestGetApiKeys $
--             newGetApiKeys
--
--         , requestDeleteBasePathMapping $
--             newDeleteBasePathMapping
--
--         , requestUpdateBasePathMapping $
--             newUpdateBasePathMapping
--
--         , requestDeleteClientCertificate $
--             newDeleteClientCertificate
--
--         , requestUpdateClientCertificate $
--             newUpdateClientCertificate
--
--         , requestGetGatewayResponse $
--             newGetGatewayResponse
--
--         , requestCreateUsagePlanKey $
--             newCreateUsagePlanKey
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
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateStage $
--             newCreateStage
--
--         , requestDeleteUsagePlanKey $
--             newDeleteUsagePlanKey
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateApiKey $
--             newCreateApiKey
--
--         , requestGetUsagePlans $
--             newGetUsagePlans
--
--         , requestPutMethod $
--             newPutMethod
--
--         , requestUpdateDomainName $
--             newUpdateDomainName
--
--         , requestDeleteDomainName $
--             newDeleteDomainName
--
--         , requestCreateResource $
--             newCreateResource
--
--         , requestDeleteMethod $
--             newDeleteMethod
--
--         , requestUpdateMethod $
--             newUpdateMethod
--
--         , requestUpdateRequestValidator $
--             newUpdateRequestValidator
--
--         , requestDeleteRequestValidator $
--             newDeleteRequestValidator
--
--         , requestGetSdkTypes $
--             newGetSdkTypes
--
--         , requestGetClientCertificates $
--             newGetClientCertificates
--
--         , requestGetModelTemplate $
--             newGetModelTemplate
--
--         , requestUpdateDocumentationVersion $
--             newUpdateDocumentationVersion
--
--         , requestDeleteDocumentationVersion $
--             newDeleteDocumentationVersion
--
--         , requestGetBasePathMappings $
--             newGetBasePathMappings
--
--         , requestGetApiKey $
--             newGetApiKey
--
--           ]

--     , testGroup "response"
--         [ responseGetResource $
--             newResource
--
--         , responseGetDeployments $
--             newGetDeploymentsResponse
--
--         , responseGetDeployment $
--             newDeployment
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseDeleteGatewayResponse $
--             newDeleteGatewayResponseResponse
--
--         , responseUpdateGatewayResponse $
--             newGatewayResponse
--
--         , responseCreateUsagePlan $
--             newUsagePlan
--
--         , responseGetDomainNames $
--             newGetDomainNamesResponse
--
--         , responseGetClientCertificate $
--             newClientCertificate
--
--         , responsePutGatewayResponse $
--             newGatewayResponse
--
--         , responseGetSdkType $
--             newSdkType
--
--         , responseGetMethodResponse $
--             newMethodResponse
--
--         , responseGetModels $
--             newGetModelsResponse
--
--         , responseGetBasePathMapping $
--             newBasePathMapping
--
--         , responseGetRequestValidators $
--             newGetRequestValidatorsResponse
--
--         , responsePutMethodResponse $
--             newMethodResponse
--
--         , responseImportRestApi $
--             newRestApi
--
--         , responseDeleteMethodResponse $
--             newDeleteMethodResponseResponse
--
--         , responseUpdateMethodResponse $
--             newMethodResponse
--
--         , responseDeleteStage $
--             newDeleteStageResponse
--
--         , responseUpdateStage $
--             newStage
--
--         , responseGetRestApis $
--             newGetRestApisResponse
--
--         , responseGetDocumentationVersions $
--             newGetDocumentationVersionsResponse
--
--         , responseCreateDeployment $
--             newDeployment
--
--         , responseGetVpcLinks $
--             newGetVpcLinksResponse
--
--         , responseCreateBasePathMapping $
--             newBasePathMapping
--
--         , responseGetIntegration $
--             newIntegration
--
--         , responseGetDocumentationParts $
--             newGetDocumentationPartsResponse
--
--         , responseUpdateAccount $
--             newAccount
--
--         , responseGetUsagePlan $
--             newUsagePlan
--
--         , responseDeleteDeployment $
--             newDeleteDeploymentResponse
--
--         , responseUpdateDeployment $
--             newDeployment
--
--         , responseGetDocumentationPart $
--             newDocumentationPart
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseUpdateResource $
--             newResource
--
--         , responseCreateRequestValidator $
--             newRequestValidator
--
--         , responseImportDocumentationParts $
--             newImportDocumentationPartsResponse
--
--         , responseGetUsage $
--             newUsage
--
--         , responseGetVpcLink $
--             newVpcLink
--
--         , responseCreateModel $
--             newModel
--
--         , responseGetIntegrationResponse $
--             newIntegrationResponse
--
--         , responseCreateDomainName $
--             newDomainName
--
--         , responseFlushStageAuthorizersCache $
--             newFlushStageAuthorizersCacheResponse
--
--         , responseGetGatewayResponses $
--             newGetGatewayResponsesResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseUpdateModel $
--             newModel
--
--         , responseGetDocumentationVersion $
--             newDocumentationVersion
--
--         , responseDeleteApiKey $
--             newDeleteApiKeyResponse
--
--         , responseUpdateApiKey $
--             newApiKey
--
--         , responseGetRestApi $
--             newRestApi
--
--         , responseGetStages $
--             newGetStagesResponse
--
--         , responsePutRestApi $
--             newRestApi
--
--         , responseGetMethod $
--             newMethod
--
--         , responseGetModel $
--             newModel
--
--         , responseUpdateRestApi $
--             newRestApi
--
--         , responseDeleteRestApi $
--             newDeleteRestApiResponse
--
--         , responseImportApiKeys $
--             newImportApiKeysResponse
--
--         , responseCreateDocumentationPart $
--             newDocumentationPart
--
--         , responseTestInvokeMethod $
--             newTestInvokeMethodResponse
--
--         , responseGetRequestValidator $
--             newRequestValidator
--
--         , responseGetDomainName $
--             newDomainName
--
--         , responseCreateVpcLink $
--             newVpcLink
--
--         , responseDeleteDocumentationPart $
--             newDeleteDocumentationPartResponse
--
--         , responseUpdateDocumentationPart $
--             newDocumentationPart
--
--         , responseGetAuthorizers $
--             newGetAuthorizersResponse
--
--         , responseCreateDocumentationVersion $
--             newDocumentationVersion
--
--         , responsePutIntegrationResponse $
--             newIntegrationResponse
--
--         , responseGetUsagePlanKeys $
--             newGetUsagePlanKeysResponse
--
--         , responseDeleteVpcLink $
--             newDeleteVpcLinkResponse
--
--         , responseUpdateVpcLink $
--             newVpcLink
--
--         , responseFlushStageCache $
--             newFlushStageCacheResponse
--
--         , responseCreateRestApi $
--             newRestApi
--
--         , responseDeleteIntegrationResponse $
--             newDeleteIntegrationResponseResponse
--
--         , responseUpdateIntegrationResponse $
--             newIntegrationResponse
--
--         , responseUpdateUsage $
--             newUsage
--
--         , responseDeleteIntegration $
--             newDeleteIntegrationResponse'
--
--         , responseUpdateIntegration $
--             newIntegration
--
--         , responseTestInvokeAuthorizer $
--             newTestInvokeAuthorizerResponse
--
--         , responseGenerateClientCertificate $
--             newClientCertificate
--
--         , responseGetResources $
--             newGetResourcesResponse
--
--         , responseGetUsagePlanKey $
--             newUsagePlanKey
--
--         , responseGetAccount $
--             newAccount
--
--         , responsePutIntegration $
--             newIntegration
--
--         , responseGetAuthorizer $
--             newAuthorizer
--
--         , responseDeleteUsagePlan $
--             newDeleteUsagePlanResponse
--
--         , responseUpdateUsagePlan $
--             newUsagePlan
--
--         , responseGetStage $
--             newStage
--
--         , responseGetExport $
--             newGetExportResponse
--
--         , responseGetSdk $
--             newGetSdkResponse
--
--         , responseGetApiKeys $
--             newGetApiKeysResponse
--
--         , responseDeleteBasePathMapping $
--             newDeleteBasePathMappingResponse
--
--         , responseUpdateBasePathMapping $
--             newBasePathMapping
--
--         , responseDeleteClientCertificate $
--             newDeleteClientCertificateResponse
--
--         , responseUpdateClientCertificate $
--             newClientCertificate
--
--         , responseGetGatewayResponse $
--             newGatewayResponse
--
--         , responseCreateUsagePlanKey $
--             newUsagePlanKey
--
--         , responseCreateAuthorizer $
--             newAuthorizer
--
--         , responseUpdateAuthorizer $
--             newAuthorizer
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateStage $
--             newStage
--
--         , responseDeleteUsagePlanKey $
--             newDeleteUsagePlanKeyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateApiKey $
--             newApiKey
--
--         , responseGetUsagePlans $
--             newGetUsagePlansResponse
--
--         , responsePutMethod $
--             newMethod
--
--         , responseUpdateDomainName $
--             newDomainName
--
--         , responseDeleteDomainName $
--             newDeleteDomainNameResponse
--
--         , responseCreateResource $
--             newResource
--
--         , responseDeleteMethod $
--             newDeleteMethodResponse'
--
--         , responseUpdateMethod $
--             newMethod
--
--         , responseUpdateRequestValidator $
--             newRequestValidator
--
--         , responseDeleteRequestValidator $
--             newDeleteRequestValidatorResponse
--
--         , responseGetSdkTypes $
--             newGetSdkTypesResponse
--
--         , responseGetClientCertificates $
--             newGetClientCertificatesResponse
--
--         , responseGetModelTemplate $
--             newGetModelTemplateResponse
--
--         , responseUpdateDocumentationVersion $
--             newDocumentationVersion
--
--         , responseDeleteDocumentationVersion $
--             newDeleteDocumentationVersionResponse
--
--         , responseGetBasePathMappings $
--             newGetBasePathMappingsResponse
--
--         , responseGetApiKey $
--             newApiKey
--
--           ]
--     ]

-- Requests

requestGetResource :: GetResource -> TestTree
requestGetResource =
  req
    "GetResource"
    "fixture/GetResource.yaml"

requestGetDeployments :: GetDeployments -> TestTree
requestGetDeployments =
  req
    "GetDeployments"
    "fixture/GetDeployments.yaml"

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

requestDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
requestDeleteGatewayResponse =
  req
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.yaml"

requestUpdateGatewayResponse :: UpdateGatewayResponse -> TestTree
requestUpdateGatewayResponse =
  req
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.yaml"

requestCreateUsagePlan :: CreateUsagePlan -> TestTree
requestCreateUsagePlan =
  req
    "CreateUsagePlan"
    "fixture/CreateUsagePlan.yaml"

requestGetDomainNames :: GetDomainNames -> TestTree
requestGetDomainNames =
  req
    "GetDomainNames"
    "fixture/GetDomainNames.yaml"

requestGetClientCertificate :: GetClientCertificate -> TestTree
requestGetClientCertificate =
  req
    "GetClientCertificate"
    "fixture/GetClientCertificate.yaml"

requestPutGatewayResponse :: PutGatewayResponse -> TestTree
requestPutGatewayResponse =
  req
    "PutGatewayResponse"
    "fixture/PutGatewayResponse.yaml"

requestGetSdkType :: GetSdkType -> TestTree
requestGetSdkType =
  req
    "GetSdkType"
    "fixture/GetSdkType.yaml"

requestGetMethodResponse :: GetMethodResponse -> TestTree
requestGetMethodResponse =
  req
    "GetMethodResponse"
    "fixture/GetMethodResponse.yaml"

requestGetModels :: GetModels -> TestTree
requestGetModels =
  req
    "GetModels"
    "fixture/GetModels.yaml"

requestGetBasePathMapping :: GetBasePathMapping -> TestTree
requestGetBasePathMapping =
  req
    "GetBasePathMapping"
    "fixture/GetBasePathMapping.yaml"

requestGetRequestValidators :: GetRequestValidators -> TestTree
requestGetRequestValidators =
  req
    "GetRequestValidators"
    "fixture/GetRequestValidators.yaml"

requestPutMethodResponse :: PutMethodResponse -> TestTree
requestPutMethodResponse =
  req
    "PutMethodResponse"
    "fixture/PutMethodResponse.yaml"

requestImportRestApi :: ImportRestApi -> TestTree
requestImportRestApi =
  req
    "ImportRestApi"
    "fixture/ImportRestApi.yaml"

requestDeleteMethodResponse :: DeleteMethodResponse -> TestTree
requestDeleteMethodResponse =
  req
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.yaml"

requestUpdateMethodResponse :: UpdateMethodResponse -> TestTree
requestUpdateMethodResponse =
  req
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.yaml"

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

requestGetRestApis :: GetRestApis -> TestTree
requestGetRestApis =
  req
    "GetRestApis"
    "fixture/GetRestApis.yaml"

requestGetDocumentationVersions :: GetDocumentationVersions -> TestTree
requestGetDocumentationVersions =
  req
    "GetDocumentationVersions"
    "fixture/GetDocumentationVersions.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestGetVpcLinks :: GetVpcLinks -> TestTree
requestGetVpcLinks =
  req
    "GetVpcLinks"
    "fixture/GetVpcLinks.yaml"

requestCreateBasePathMapping :: CreateBasePathMapping -> TestTree
requestCreateBasePathMapping =
  req
    "CreateBasePathMapping"
    "fixture/CreateBasePathMapping.yaml"

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration =
  req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

requestGetDocumentationParts :: GetDocumentationParts -> TestTree
requestGetDocumentationParts =
  req
    "GetDocumentationParts"
    "fixture/GetDocumentationParts.yaml"

requestUpdateAccount :: UpdateAccount -> TestTree
requestUpdateAccount =
  req
    "UpdateAccount"
    "fixture/UpdateAccount.yaml"

requestGetUsagePlan :: GetUsagePlan -> TestTree
requestGetUsagePlan =
  req
    "GetUsagePlan"
    "fixture/GetUsagePlan.yaml"

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

requestGetDocumentationPart :: GetDocumentationPart -> TestTree
requestGetDocumentationPart =
  req
    "GetDocumentationPart"
    "fixture/GetDocumentationPart.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestCreateRequestValidator :: CreateRequestValidator -> TestTree
requestCreateRequestValidator =
  req
    "CreateRequestValidator"
    "fixture/CreateRequestValidator.yaml"

requestImportDocumentationParts :: ImportDocumentationParts -> TestTree
requestImportDocumentationParts =
  req
    "ImportDocumentationParts"
    "fixture/ImportDocumentationParts.yaml"

requestGetUsage :: GetUsage -> TestTree
requestGetUsage =
  req
    "GetUsage"
    "fixture/GetUsage.yaml"

requestGetVpcLink :: GetVpcLink -> TestTree
requestGetVpcLink =
  req
    "GetVpcLink"
    "fixture/GetVpcLink.yaml"

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

requestFlushStageAuthorizersCache :: FlushStageAuthorizersCache -> TestTree
requestFlushStageAuthorizersCache =
  req
    "FlushStageAuthorizersCache"
    "fixture/FlushStageAuthorizersCache.yaml"

requestGetGatewayResponses :: GetGatewayResponses -> TestTree
requestGetGatewayResponses =
  req
    "GetGatewayResponses"
    "fixture/GetGatewayResponses.yaml"

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

requestGetDocumentationVersion :: GetDocumentationVersion -> TestTree
requestGetDocumentationVersion =
  req
    "GetDocumentationVersion"
    "fixture/GetDocumentationVersion.yaml"

requestDeleteApiKey :: DeleteApiKey -> TestTree
requestDeleteApiKey =
  req
    "DeleteApiKey"
    "fixture/DeleteApiKey.yaml"

requestUpdateApiKey :: UpdateApiKey -> TestTree
requestUpdateApiKey =
  req
    "UpdateApiKey"
    "fixture/UpdateApiKey.yaml"

requestGetRestApi :: GetRestApi -> TestTree
requestGetRestApi =
  req
    "GetRestApi"
    "fixture/GetRestApi.yaml"

requestGetStages :: GetStages -> TestTree
requestGetStages =
  req
    "GetStages"
    "fixture/GetStages.yaml"

requestPutRestApi :: PutRestApi -> TestTree
requestPutRestApi =
  req
    "PutRestApi"
    "fixture/PutRestApi.yaml"

requestGetMethod :: GetMethod -> TestTree
requestGetMethod =
  req
    "GetMethod"
    "fixture/GetMethod.yaml"

requestGetModel :: GetModel -> TestTree
requestGetModel =
  req
    "GetModel"
    "fixture/GetModel.yaml"

requestUpdateRestApi :: UpdateRestApi -> TestTree
requestUpdateRestApi =
  req
    "UpdateRestApi"
    "fixture/UpdateRestApi.yaml"

requestDeleteRestApi :: DeleteRestApi -> TestTree
requestDeleteRestApi =
  req
    "DeleteRestApi"
    "fixture/DeleteRestApi.yaml"

requestImportApiKeys :: ImportApiKeys -> TestTree
requestImportApiKeys =
  req
    "ImportApiKeys"
    "fixture/ImportApiKeys.yaml"

requestCreateDocumentationPart :: CreateDocumentationPart -> TestTree
requestCreateDocumentationPart =
  req
    "CreateDocumentationPart"
    "fixture/CreateDocumentationPart.yaml"

requestTestInvokeMethod :: TestInvokeMethod -> TestTree
requestTestInvokeMethod =
  req
    "TestInvokeMethod"
    "fixture/TestInvokeMethod.yaml"

requestGetRequestValidator :: GetRequestValidator -> TestTree
requestGetRequestValidator =
  req
    "GetRequestValidator"
    "fixture/GetRequestValidator.yaml"

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

requestDeleteDocumentationPart :: DeleteDocumentationPart -> TestTree
requestDeleteDocumentationPart =
  req
    "DeleteDocumentationPart"
    "fixture/DeleteDocumentationPart.yaml"

requestUpdateDocumentationPart :: UpdateDocumentationPart -> TestTree
requestUpdateDocumentationPart =
  req
    "UpdateDocumentationPart"
    "fixture/UpdateDocumentationPart.yaml"

requestGetAuthorizers :: GetAuthorizers -> TestTree
requestGetAuthorizers =
  req
    "GetAuthorizers"
    "fixture/GetAuthorizers.yaml"

requestCreateDocumentationVersion :: CreateDocumentationVersion -> TestTree
requestCreateDocumentationVersion =
  req
    "CreateDocumentationVersion"
    "fixture/CreateDocumentationVersion.yaml"

requestPutIntegrationResponse :: PutIntegrationResponse -> TestTree
requestPutIntegrationResponse =
  req
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.yaml"

requestGetUsagePlanKeys :: GetUsagePlanKeys -> TestTree
requestGetUsagePlanKeys =
  req
    "GetUsagePlanKeys"
    "fixture/GetUsagePlanKeys.yaml"

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

requestFlushStageCache :: FlushStageCache -> TestTree
requestFlushStageCache =
  req
    "FlushStageCache"
    "fixture/FlushStageCache.yaml"

requestCreateRestApi :: CreateRestApi -> TestTree
requestCreateRestApi =
  req
    "CreateRestApi"
    "fixture/CreateRestApi.yaml"

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

requestUpdateUsage :: UpdateUsage -> TestTree
requestUpdateUsage =
  req
    "UpdateUsage"
    "fixture/UpdateUsage.yaml"

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

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer =
  req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

requestGenerateClientCertificate :: GenerateClientCertificate -> TestTree
requestGenerateClientCertificate =
  req
    "GenerateClientCertificate"
    "fixture/GenerateClientCertificate.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources =
  req
    "GetResources"
    "fixture/GetResources.yaml"

requestGetUsagePlanKey :: GetUsagePlanKey -> TestTree
requestGetUsagePlanKey =
  req
    "GetUsagePlanKey"
    "fixture/GetUsagePlanKey.yaml"

requestGetAccount :: GetAccount -> TestTree
requestGetAccount =
  req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestPutIntegration :: PutIntegration -> TestTree
requestPutIntegration =
  req
    "PutIntegration"
    "fixture/PutIntegration.yaml"

requestGetAuthorizer :: GetAuthorizer -> TestTree
requestGetAuthorizer =
  req
    "GetAuthorizer"
    "fixture/GetAuthorizer.yaml"

requestDeleteUsagePlan :: DeleteUsagePlan -> TestTree
requestDeleteUsagePlan =
  req
    "DeleteUsagePlan"
    "fixture/DeleteUsagePlan.yaml"

requestUpdateUsagePlan :: UpdateUsagePlan -> TestTree
requestUpdateUsagePlan =
  req
    "UpdateUsagePlan"
    "fixture/UpdateUsagePlan.yaml"

requestGetStage :: GetStage -> TestTree
requestGetStage =
  req
    "GetStage"
    "fixture/GetStage.yaml"

requestGetExport :: GetExport -> TestTree
requestGetExport =
  req
    "GetExport"
    "fixture/GetExport.yaml"

requestGetSdk :: GetSdk -> TestTree
requestGetSdk =
  req
    "GetSdk"
    "fixture/GetSdk.yaml"

requestGetApiKeys :: GetApiKeys -> TestTree
requestGetApiKeys =
  req
    "GetApiKeys"
    "fixture/GetApiKeys.yaml"

requestDeleteBasePathMapping :: DeleteBasePathMapping -> TestTree
requestDeleteBasePathMapping =
  req
    "DeleteBasePathMapping"
    "fixture/DeleteBasePathMapping.yaml"

requestUpdateBasePathMapping :: UpdateBasePathMapping -> TestTree
requestUpdateBasePathMapping =
  req
    "UpdateBasePathMapping"
    "fixture/UpdateBasePathMapping.yaml"

requestDeleteClientCertificate :: DeleteClientCertificate -> TestTree
requestDeleteClientCertificate =
  req
    "DeleteClientCertificate"
    "fixture/DeleteClientCertificate.yaml"

requestUpdateClientCertificate :: UpdateClientCertificate -> TestTree
requestUpdateClientCertificate =
  req
    "UpdateClientCertificate"
    "fixture/UpdateClientCertificate.yaml"

requestGetGatewayResponse :: GetGatewayResponse -> TestTree
requestGetGatewayResponse =
  req
    "GetGatewayResponse"
    "fixture/GetGatewayResponse.yaml"

requestCreateUsagePlanKey :: CreateUsagePlanKey -> TestTree
requestCreateUsagePlanKey =
  req
    "CreateUsagePlanKey"
    "fixture/CreateUsagePlanKey.yaml"

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

requestDeleteUsagePlanKey :: DeleteUsagePlanKey -> TestTree
requestDeleteUsagePlanKey =
  req
    "DeleteUsagePlanKey"
    "fixture/DeleteUsagePlanKey.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateApiKey :: CreateApiKey -> TestTree
requestCreateApiKey =
  req
    "CreateApiKey"
    "fixture/CreateApiKey.yaml"

requestGetUsagePlans :: GetUsagePlans -> TestTree
requestGetUsagePlans =
  req
    "GetUsagePlans"
    "fixture/GetUsagePlans.yaml"

requestPutMethod :: PutMethod -> TestTree
requestPutMethod =
  req
    "PutMethod"
    "fixture/PutMethod.yaml"

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

requestCreateResource :: CreateResource -> TestTree
requestCreateResource =
  req
    "CreateResource"
    "fixture/CreateResource.yaml"

requestDeleteMethod :: DeleteMethod -> TestTree
requestDeleteMethod =
  req
    "DeleteMethod"
    "fixture/DeleteMethod.yaml"

requestUpdateMethod :: UpdateMethod -> TestTree
requestUpdateMethod =
  req
    "UpdateMethod"
    "fixture/UpdateMethod.yaml"

requestUpdateRequestValidator :: UpdateRequestValidator -> TestTree
requestUpdateRequestValidator =
  req
    "UpdateRequestValidator"
    "fixture/UpdateRequestValidator.yaml"

requestDeleteRequestValidator :: DeleteRequestValidator -> TestTree
requestDeleteRequestValidator =
  req
    "DeleteRequestValidator"
    "fixture/DeleteRequestValidator.yaml"

requestGetSdkTypes :: GetSdkTypes -> TestTree
requestGetSdkTypes =
  req
    "GetSdkTypes"
    "fixture/GetSdkTypes.yaml"

requestGetClientCertificates :: GetClientCertificates -> TestTree
requestGetClientCertificates =
  req
    "GetClientCertificates"
    "fixture/GetClientCertificates.yaml"

requestGetModelTemplate :: GetModelTemplate -> TestTree
requestGetModelTemplate =
  req
    "GetModelTemplate"
    "fixture/GetModelTemplate.yaml"

requestUpdateDocumentationVersion :: UpdateDocumentationVersion -> TestTree
requestUpdateDocumentationVersion =
  req
    "UpdateDocumentationVersion"
    "fixture/UpdateDocumentationVersion.yaml"

requestDeleteDocumentationVersion :: DeleteDocumentationVersion -> TestTree
requestDeleteDocumentationVersion =
  req
    "DeleteDocumentationVersion"
    "fixture/DeleteDocumentationVersion.yaml"

requestGetBasePathMappings :: GetBasePathMappings -> TestTree
requestGetBasePathMappings =
  req
    "GetBasePathMappings"
    "fixture/GetBasePathMappings.yaml"

requestGetApiKey :: GetApiKey -> TestTree
requestGetApiKey =
  req
    "GetApiKey"
    "fixture/GetApiKey.yaml"

-- Responses

responseGetResource :: Resource -> TestTree
responseGetResource =
  res
    "GetResourceResponse"
    "fixture/GetResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResource)

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments =
  res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployments)

responseGetDeployment :: Deployment -> TestTree
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

responseDeleteGatewayResponse :: DeleteGatewayResponseResponse -> TestTree
responseDeleteGatewayResponse =
  res
    "DeleteGatewayResponseResponse"
    "fixture/DeleteGatewayResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGatewayResponse)

responseUpdateGatewayResponse :: GatewayResponse -> TestTree
responseUpdateGatewayResponse =
  res
    "UpdateGatewayResponseResponse"
    "fixture/UpdateGatewayResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGatewayResponse)

responseCreateUsagePlan :: UsagePlan -> TestTree
responseCreateUsagePlan =
  res
    "CreateUsagePlanResponse"
    "fixture/CreateUsagePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUsagePlan)

responseGetDomainNames :: GetDomainNamesResponse -> TestTree
responseGetDomainNames =
  res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainNames)

responseGetClientCertificate :: ClientCertificate -> TestTree
responseGetClientCertificate =
  res
    "GetClientCertificateResponse"
    "fixture/GetClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClientCertificate)

responsePutGatewayResponse :: GatewayResponse -> TestTree
responsePutGatewayResponse =
  res
    "PutGatewayResponseResponse"
    "fixture/PutGatewayResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutGatewayResponse)

responseGetSdkType :: SdkType -> TestTree
responseGetSdkType =
  res
    "GetSdkTypeResponse"
    "fixture/GetSdkTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSdkType)

responseGetMethodResponse :: MethodResponse -> TestTree
responseGetMethodResponse =
  res
    "GetMethodResponseResponse"
    "fixture/GetMethodResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMethodResponse)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels =
  res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModels)

responseGetBasePathMapping :: BasePathMapping -> TestTree
responseGetBasePathMapping =
  res
    "GetBasePathMappingResponse"
    "fixture/GetBasePathMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBasePathMapping)

responseGetRequestValidators :: GetRequestValidatorsResponse -> TestTree
responseGetRequestValidators =
  res
    "GetRequestValidatorsResponse"
    "fixture/GetRequestValidatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRequestValidators)

responsePutMethodResponse :: MethodResponse -> TestTree
responsePutMethodResponse =
  res
    "PutMethodResponseResponse"
    "fixture/PutMethodResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMethodResponse)

responseImportRestApi :: RestApi -> TestTree
responseImportRestApi =
  res
    "ImportRestApiResponse"
    "fixture/ImportRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportRestApi)

responseDeleteMethodResponse :: DeleteMethodResponseResponse -> TestTree
responseDeleteMethodResponse =
  res
    "DeleteMethodResponseResponse"
    "fixture/DeleteMethodResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMethodResponse)

responseUpdateMethodResponse :: MethodResponse -> TestTree
responseUpdateMethodResponse =
  res
    "UpdateMethodResponseResponse"
    "fixture/UpdateMethodResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMethodResponse)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage =
  res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStage)

responseUpdateStage :: Stage -> TestTree
responseUpdateStage =
  res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStage)

responseGetRestApis :: GetRestApisResponse -> TestTree
responseGetRestApis =
  res
    "GetRestApisResponse"
    "fixture/GetRestApisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRestApis)

responseGetDocumentationVersions :: GetDocumentationVersionsResponse -> TestTree
responseGetDocumentationVersions =
  res
    "GetDocumentationVersionsResponse"
    "fixture/GetDocumentationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentationVersions)

responseCreateDeployment :: Deployment -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseGetVpcLinks :: GetVpcLinksResponse -> TestTree
responseGetVpcLinks =
  res
    "GetVpcLinksResponse"
    "fixture/GetVpcLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpcLinks)

responseCreateBasePathMapping :: BasePathMapping -> TestTree
responseCreateBasePathMapping =
  res
    "CreateBasePathMappingResponse"
    "fixture/CreateBasePathMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBasePathMapping)

responseGetIntegration :: Integration -> TestTree
responseGetIntegration =
  res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegration)

responseGetDocumentationParts :: GetDocumentationPartsResponse -> TestTree
responseGetDocumentationParts =
  res
    "GetDocumentationPartsResponse"
    "fixture/GetDocumentationPartsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentationParts)

responseUpdateAccount :: Account -> TestTree
responseUpdateAccount =
  res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAccount)

responseGetUsagePlan :: UsagePlan -> TestTree
responseGetUsagePlan =
  res
    "GetUsagePlanResponse"
    "fixture/GetUsagePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsagePlan)

responseDeleteDeployment :: DeleteDeploymentResponse -> TestTree
responseDeleteDeployment =
  res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeployment)

responseUpdateDeployment :: Deployment -> TestTree
responseUpdateDeployment =
  res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeployment)

responseGetDocumentationPart :: DocumentationPart -> TestTree
responseGetDocumentationPart =
  res
    "GetDocumentationPartResponse"
    "fixture/GetDocumentationPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentationPart)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResource)

responseUpdateResource :: Resource -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResource)

responseCreateRequestValidator :: RequestValidator -> TestTree
responseCreateRequestValidator =
  res
    "CreateRequestValidatorResponse"
    "fixture/CreateRequestValidatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRequestValidator)

responseImportDocumentationParts :: ImportDocumentationPartsResponse -> TestTree
responseImportDocumentationParts =
  res
    "ImportDocumentationPartsResponse"
    "fixture/ImportDocumentationPartsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportDocumentationParts)

responseGetUsage :: Usage -> TestTree
responseGetUsage =
  res
    "GetUsageResponse"
    "fixture/GetUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsage)

responseGetVpcLink :: VpcLink -> TestTree
responseGetVpcLink =
  res
    "GetVpcLinkResponse"
    "fixture/GetVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVpcLink)

responseCreateModel :: Model -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

responseGetIntegrationResponse :: IntegrationResponse -> TestTree
responseGetIntegrationResponse =
  res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegrationResponse)

responseCreateDomainName :: DomainName -> TestTree
responseCreateDomainName =
  res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainName)

responseFlushStageAuthorizersCache :: FlushStageAuthorizersCacheResponse -> TestTree
responseFlushStageAuthorizersCache =
  res
    "FlushStageAuthorizersCacheResponse"
    "fixture/FlushStageAuthorizersCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FlushStageAuthorizersCache)

responseGetGatewayResponses :: GetGatewayResponsesResponse -> TestTree
responseGetGatewayResponses =
  res
    "GetGatewayResponsesResponse"
    "fixture/GetGatewayResponsesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGatewayResponses)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseUpdateModel :: Model -> TestTree
responseUpdateModel =
  res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModel)

responseGetDocumentationVersion :: DocumentationVersion -> TestTree
responseGetDocumentationVersion =
  res
    "GetDocumentationVersionResponse"
    "fixture/GetDocumentationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentationVersion)

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey =
  res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApiKey)

responseUpdateApiKey :: ApiKey -> TestTree
responseUpdateApiKey =
  res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApiKey)

responseGetRestApi :: RestApi -> TestTree
responseGetRestApi =
  res
    "GetRestApiResponse"
    "fixture/GetRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRestApi)

responseGetStages :: GetStagesResponse -> TestTree
responseGetStages =
  res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStages)

responsePutRestApi :: RestApi -> TestTree
responsePutRestApi =
  res
    "PutRestApiResponse"
    "fixture/PutRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRestApi)

responseGetMethod :: Method -> TestTree
responseGetMethod =
  res
    "GetMethodResponse"
    "fixture/GetMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMethod)

responseGetModel :: Model -> TestTree
responseGetModel =
  res
    "GetModelResponse"
    "fixture/GetModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModel)

responseUpdateRestApi :: RestApi -> TestTree
responseUpdateRestApi =
  res
    "UpdateRestApiResponse"
    "fixture/UpdateRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRestApi)

responseDeleteRestApi :: DeleteRestApiResponse -> TestTree
responseDeleteRestApi =
  res
    "DeleteRestApiResponse"
    "fixture/DeleteRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRestApi)

responseImportApiKeys :: ImportApiKeysResponse -> TestTree
responseImportApiKeys =
  res
    "ImportApiKeysResponse"
    "fixture/ImportApiKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportApiKeys)

responseCreateDocumentationPart :: DocumentationPart -> TestTree
responseCreateDocumentationPart =
  res
    "CreateDocumentationPartResponse"
    "fixture/CreateDocumentationPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDocumentationPart)

responseTestInvokeMethod :: TestInvokeMethodResponse -> TestTree
responseTestInvokeMethod =
  res
    "TestInvokeMethodResponse"
    "fixture/TestInvokeMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestInvokeMethod)

responseGetRequestValidator :: RequestValidator -> TestTree
responseGetRequestValidator =
  res
    "GetRequestValidatorResponse"
    "fixture/GetRequestValidatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRequestValidator)

responseGetDomainName :: DomainName -> TestTree
responseGetDomainName =
  res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomainName)

responseCreateVpcLink :: VpcLink -> TestTree
responseCreateVpcLink =
  res
    "CreateVpcLinkResponse"
    "fixture/CreateVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcLink)

responseDeleteDocumentationPart :: DeleteDocumentationPartResponse -> TestTree
responseDeleteDocumentationPart =
  res
    "DeleteDocumentationPartResponse"
    "fixture/DeleteDocumentationPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocumentationPart)

responseUpdateDocumentationPart :: DocumentationPart -> TestTree
responseUpdateDocumentationPart =
  res
    "UpdateDocumentationPartResponse"
    "fixture/UpdateDocumentationPartResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentationPart)

responseGetAuthorizers :: GetAuthorizersResponse -> TestTree
responseGetAuthorizers =
  res
    "GetAuthorizersResponse"
    "fixture/GetAuthorizersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizers)

responseCreateDocumentationVersion :: DocumentationVersion -> TestTree
responseCreateDocumentationVersion =
  res
    "CreateDocumentationVersionResponse"
    "fixture/CreateDocumentationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDocumentationVersion)

responsePutIntegrationResponse :: IntegrationResponse -> TestTree
responsePutIntegrationResponse =
  res
    "PutIntegrationResponseResponse"
    "fixture/PutIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutIntegrationResponse)

responseGetUsagePlanKeys :: GetUsagePlanKeysResponse -> TestTree
responseGetUsagePlanKeys =
  res
    "GetUsagePlanKeysResponse"
    "fixture/GetUsagePlanKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsagePlanKeys)

responseDeleteVpcLink :: DeleteVpcLinkResponse -> TestTree
responseDeleteVpcLink =
  res
    "DeleteVpcLinkResponse"
    "fixture/DeleteVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcLink)

responseUpdateVpcLink :: VpcLink -> TestTree
responseUpdateVpcLink =
  res
    "UpdateVpcLinkResponse"
    "fixture/UpdateVpcLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVpcLink)

responseFlushStageCache :: FlushStageCacheResponse -> TestTree
responseFlushStageCache =
  res
    "FlushStageCacheResponse"
    "fixture/FlushStageCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy FlushStageCache)

responseCreateRestApi :: RestApi -> TestTree
responseCreateRestApi =
  res
    "CreateRestApiResponse"
    "fixture/CreateRestApiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRestApi)

responseDeleteIntegrationResponse :: DeleteIntegrationResponseResponse -> TestTree
responseDeleteIntegrationResponse =
  res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegrationResponse)

responseUpdateIntegrationResponse :: IntegrationResponse -> TestTree
responseUpdateIntegrationResponse =
  res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIntegrationResponse)

responseUpdateUsage :: Usage -> TestTree
responseUpdateUsage =
  res
    "UpdateUsageResponse"
    "fixture/UpdateUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUsage)

responseDeleteIntegration :: DeleteIntegrationResponse' -> TestTree
responseDeleteIntegration =
  res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegration)

responseUpdateIntegration :: Integration -> TestTree
responseUpdateIntegration =
  res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIntegration)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestInvokeAuthorizer)

responseGenerateClientCertificate :: ClientCertificate -> TestTree
responseGenerateClientCertificate =
  res
    "GenerateClientCertificateResponse"
    "fixture/GenerateClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateClientCertificate)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResources)

responseGetUsagePlanKey :: UsagePlanKey -> TestTree
responseGetUsagePlanKey =
  res
    "GetUsagePlanKeyResponse"
    "fixture/GetUsagePlanKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsagePlanKey)

responseGetAccount :: Account -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccount)

responsePutIntegration :: Integration -> TestTree
responsePutIntegration =
  res
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutIntegration)

responseGetAuthorizer :: Authorizer -> TestTree
responseGetAuthorizer =
  res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAuthorizer)

responseDeleteUsagePlan :: DeleteUsagePlanResponse -> TestTree
responseDeleteUsagePlan =
  res
    "DeleteUsagePlanResponse"
    "fixture/DeleteUsagePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUsagePlan)

responseUpdateUsagePlan :: UsagePlan -> TestTree
responseUpdateUsagePlan =
  res
    "UpdateUsagePlanResponse"
    "fixture/UpdateUsagePlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUsagePlan)

responseGetStage :: Stage -> TestTree
responseGetStage =
  res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStage)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport =
  res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExport)

responseGetSdk :: GetSdkResponse -> TestTree
responseGetSdk =
  res
    "GetSdkResponse"
    "fixture/GetSdkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSdk)

responseGetApiKeys :: GetApiKeysResponse -> TestTree
responseGetApiKeys =
  res
    "GetApiKeysResponse"
    "fixture/GetApiKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiKeys)

responseDeleteBasePathMapping :: DeleteBasePathMappingResponse -> TestTree
responseDeleteBasePathMapping =
  res
    "DeleteBasePathMappingResponse"
    "fixture/DeleteBasePathMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBasePathMapping)

responseUpdateBasePathMapping :: BasePathMapping -> TestTree
responseUpdateBasePathMapping =
  res
    "UpdateBasePathMappingResponse"
    "fixture/UpdateBasePathMappingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBasePathMapping)

responseDeleteClientCertificate :: DeleteClientCertificateResponse -> TestTree
responseDeleteClientCertificate =
  res
    "DeleteClientCertificateResponse"
    "fixture/DeleteClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClientCertificate)

responseUpdateClientCertificate :: ClientCertificate -> TestTree
responseUpdateClientCertificate =
  res
    "UpdateClientCertificateResponse"
    "fixture/UpdateClientCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClientCertificate)

responseGetGatewayResponse :: GatewayResponse -> TestTree
responseGetGatewayResponse =
  res
    "GetGatewayResponseResponse"
    "fixture/GetGatewayResponseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGatewayResponse)

responseCreateUsagePlanKey :: UsagePlanKey -> TestTree
responseCreateUsagePlanKey =
  res
    "CreateUsagePlanKeyResponse"
    "fixture/CreateUsagePlanKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUsagePlanKey)

responseCreateAuthorizer :: Authorizer -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAuthorizer)

responseUpdateAuthorizer :: Authorizer -> TestTree
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

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateStage :: Stage -> TestTree
responseCreateStage =
  res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStage)

responseDeleteUsagePlanKey :: DeleteUsagePlanKeyResponse -> TestTree
responseDeleteUsagePlanKey =
  res
    "DeleteUsagePlanKeyResponse"
    "fixture/DeleteUsagePlanKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUsagePlanKey)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateApiKey :: ApiKey -> TestTree
responseCreateApiKey =
  res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApiKey)

responseGetUsagePlans :: GetUsagePlansResponse -> TestTree
responseGetUsagePlans =
  res
    "GetUsagePlansResponse"
    "fixture/GetUsagePlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUsagePlans)

responsePutMethod :: Method -> TestTree
responsePutMethod =
  res
    "PutMethodResponse"
    "fixture/PutMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMethod)

responseUpdateDomainName :: DomainName -> TestTree
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

responseCreateResource :: Resource -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResource)

responseDeleteMethod :: DeleteMethodResponse' -> TestTree
responseDeleteMethod =
  res
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMethod)

responseUpdateMethod :: Method -> TestTree
responseUpdateMethod =
  res
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMethod)

responseUpdateRequestValidator :: RequestValidator -> TestTree
responseUpdateRequestValidator =
  res
    "UpdateRequestValidatorResponse"
    "fixture/UpdateRequestValidatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRequestValidator)

responseDeleteRequestValidator :: DeleteRequestValidatorResponse -> TestTree
responseDeleteRequestValidator =
  res
    "DeleteRequestValidatorResponse"
    "fixture/DeleteRequestValidatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRequestValidator)

responseGetSdkTypes :: GetSdkTypesResponse -> TestTree
responseGetSdkTypes =
  res
    "GetSdkTypesResponse"
    "fixture/GetSdkTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSdkTypes)

responseGetClientCertificates :: GetClientCertificatesResponse -> TestTree
responseGetClientCertificates =
  res
    "GetClientCertificatesResponse"
    "fixture/GetClientCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetClientCertificates)

responseGetModelTemplate :: GetModelTemplateResponse -> TestTree
responseGetModelTemplate =
  res
    "GetModelTemplateResponse"
    "fixture/GetModelTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModelTemplate)

responseUpdateDocumentationVersion :: DocumentationVersion -> TestTree
responseUpdateDocumentationVersion =
  res
    "UpdateDocumentationVersionResponse"
    "fixture/UpdateDocumentationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDocumentationVersion)

responseDeleteDocumentationVersion :: DeleteDocumentationVersionResponse -> TestTree
responseDeleteDocumentationVersion =
  res
    "DeleteDocumentationVersionResponse"
    "fixture/DeleteDocumentationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDocumentationVersion)

responseGetBasePathMappings :: GetBasePathMappingsResponse -> TestTree
responseGetBasePathMappings =
  res
    "GetBasePathMappingsResponse"
    "fixture/GetBasePathMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBasePathMappings)

responseGetApiKey :: ApiKey -> TestTree
responseGetApiKey =
  res
    "GetApiKeyResponse"
    "fixture/GetApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApiKey)
