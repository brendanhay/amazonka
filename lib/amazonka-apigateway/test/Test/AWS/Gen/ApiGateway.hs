{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ApiGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ApiGateway where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ApiGateway
import Test.AWS.ApiGateway.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetResource $
--             mkGetResource
--
--         , requestGetDeployments $
--             mkGetDeployments
--
--         , requestGetDeployment $
--             mkGetDeployment
--
--         , requestGetTags $
--             mkGetTags
--
--         , requestDeleteGatewayResponse $
--             mkDeleteGatewayResponse
--
--         , requestUpdateGatewayResponse $
--             mkUpdateGatewayResponse
--
--         , requestCreateUsagePlan $
--             mkCreateUsagePlan
--
--         , requestGetDomainNames $
--             mkGetDomainNames
--
--         , requestGetClientCertificate $
--             mkGetClientCertificate
--
--         , requestPutGatewayResponse $
--             mkPutGatewayResponse
--
--         , requestGetSdkType $
--             mkGetSdkType
--
--         , requestGetMethodResponse $
--             mkGetMethodResponse
--
--         , requestGetModels $
--             mkGetModels
--
--         , requestGetBasePathMapping $
--             mkGetBasePathMapping
--
--         , requestGetRequestValidators $
--             mkGetRequestValidators
--
--         , requestPutMethodResponse $
--             mkPutMethodResponse
--
--         , requestImportRestApi $
--             mkImportRestApi
--
--         , requestDeleteMethodResponse $
--             mkDeleteMethodResponse
--
--         , requestUpdateMethodResponse $
--             mkUpdateMethodResponse
--
--         , requestDeleteStage $
--             mkDeleteStage
--
--         , requestUpdateStage $
--             mkUpdateStage
--
--         , requestGetRestApis $
--             mkGetRestApis
--
--         , requestGetDocumentationVersions $
--             mkGetDocumentationVersions
--
--         , requestCreateDeployment $
--             mkCreateDeployment
--
--         , requestGetVpcLinks $
--             mkGetVpcLinks
--
--         , requestCreateBasePathMapping $
--             mkCreateBasePathMapping
--
--         , requestGetIntegration $
--             mkGetIntegration
--
--         , requestGetDocumentationParts $
--             mkGetDocumentationParts
--
--         , requestUpdateAccount $
--             mkUpdateAccount
--
--         , requestGetUsagePlan $
--             mkGetUsagePlan
--
--         , requestDeleteDeployment $
--             mkDeleteDeployment
--
--         , requestUpdateDeployment $
--             mkUpdateDeployment
--
--         , requestGetDocumentationPart $
--             mkGetDocumentationPart
--
--         , requestDeleteResource $
--             mkDeleteResource
--
--         , requestUpdateResource $
--             mkUpdateResource
--
--         , requestCreateRequestValidator $
--             mkCreateRequestValidator
--
--         , requestImportDocumentationParts $
--             mkImportDocumentationParts
--
--         , requestGetUsage $
--             mkGetUsage
--
--         , requestGetVpcLink $
--             mkGetVpcLink
--
--         , requestCreateModel $
--             mkCreateModel
--
--         , requestGetIntegrationResponse $
--             mkGetIntegrationResponse
--
--         , requestCreateDomainName $
--             mkCreateDomainName
--
--         , requestFlushStageAuthorizersCache $
--             mkFlushStageAuthorizersCache
--
--         , requestGetGatewayResponses $
--             mkGetGatewayResponses
--
--         , requestDeleteModel $
--             mkDeleteModel
--
--         , requestUpdateModel $
--             mkUpdateModel
--
--         , requestGetDocumentationVersion $
--             mkGetDocumentationVersion
--
--         , requestDeleteApiKey $
--             mkDeleteApiKey
--
--         , requestUpdateApiKey $
--             mkUpdateApiKey
--
--         , requestGetRestApi $
--             mkGetRestApi
--
--         , requestGetStages $
--             mkGetStages
--
--         , requestPutRestApi $
--             mkPutRestApi
--
--         , requestGetMethod $
--             mkGetMethod
--
--         , requestGetModel $
--             mkGetModel
--
--         , requestUpdateRestApi $
--             mkUpdateRestApi
--
--         , requestDeleteRestApi $
--             mkDeleteRestApi
--
--         , requestImportApiKeys $
--             mkImportApiKeys
--
--         , requestCreateDocumentationPart $
--             mkCreateDocumentationPart
--
--         , requestTestInvokeMethod $
--             mkTestInvokeMethod
--
--         , requestGetRequestValidator $
--             mkGetRequestValidator
--
--         , requestGetDomainName $
--             mkGetDomainName
--
--         , requestCreateVpcLink $
--             mkCreateVpcLink
--
--         , requestDeleteDocumentationPart $
--             mkDeleteDocumentationPart
--
--         , requestUpdateDocumentationPart $
--             mkUpdateDocumentationPart
--
--         , requestGetAuthorizers $
--             mkGetAuthorizers
--
--         , requestCreateDocumentationVersion $
--             mkCreateDocumentationVersion
--
--         , requestPutIntegrationResponse $
--             mkPutIntegrationResponse
--
--         , requestGetUsagePlanKeys $
--             mkGetUsagePlanKeys
--
--         , requestDeleteVpcLink $
--             mkDeleteVpcLink
--
--         , requestUpdateVpcLink $
--             mkUpdateVpcLink
--
--         , requestFlushStageCache $
--             mkFlushStageCache
--
--         , requestCreateRestApi $
--             mkCreateRestApi
--
--         , requestDeleteIntegrationResponse $
--             mkDeleteIntegrationResponse
--
--         , requestUpdateIntegrationResponse $
--             mkUpdateIntegrationResponse
--
--         , requestUpdateUsage $
--             mkUpdateUsage
--
--         , requestDeleteIntegration $
--             mkDeleteIntegration
--
--         , requestUpdateIntegration $
--             mkUpdateIntegration
--
--         , requestTestInvokeAuthorizer $
--             mkTestInvokeAuthorizer
--
--         , requestGenerateClientCertificate $
--             mkGenerateClientCertificate
--
--         , requestGetResources $
--             mkGetResources
--
--         , requestGetUsagePlanKey $
--             mkGetUsagePlanKey
--
--         , requestGetAccount $
--             mkGetAccount
--
--         , requestPutIntegration $
--             mkPutIntegration
--
--         , requestGetAuthorizer $
--             mkGetAuthorizer
--
--         , requestDeleteUsagePlan $
--             mkDeleteUsagePlan
--
--         , requestUpdateUsagePlan $
--             mkUpdateUsagePlan
--
--         , requestGetStage $
--             mkGetStage
--
--         , requestGetExport $
--             mkGetExport
--
--         , requestGetSdk $
--             mkGetSdk
--
--         , requestGetApiKeys $
--             mkGetApiKeys
--
--         , requestDeleteBasePathMapping $
--             mkDeleteBasePathMapping
--
--         , requestUpdateBasePathMapping $
--             mkUpdateBasePathMapping
--
--         , requestDeleteClientCertificate $
--             mkDeleteClientCertificate
--
--         , requestUpdateClientCertificate $
--             mkUpdateClientCertificate
--
--         , requestGetGatewayResponse $
--             mkGetGatewayResponse
--
--         , requestCreateUsagePlanKey $
--             mkCreateUsagePlanKey
--
--         , requestCreateAuthorizer $
--             mkCreateAuthorizer
--
--         , requestUpdateAuthorizer $
--             mkUpdateAuthorizer
--
--         , requestDeleteAuthorizer $
--             mkDeleteAuthorizer
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestCreateStage $
--             mkCreateStage
--
--         , requestDeleteUsagePlanKey $
--             mkDeleteUsagePlanKey
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestCreateApiKey $
--             mkCreateApiKey
--
--         , requestGetUsagePlans $
--             mkGetUsagePlans
--
--         , requestPutMethod $
--             mkPutMethod
--
--         , requestUpdateDomainName $
--             mkUpdateDomainName
--
--         , requestDeleteDomainName $
--             mkDeleteDomainName
--
--         , requestCreateResource $
--             mkCreateResource
--
--         , requestDeleteMethod $
--             mkDeleteMethod
--
--         , requestUpdateMethod $
--             mkUpdateMethod
--
--         , requestUpdateRequestValidator $
--             mkUpdateRequestValidator
--
--         , requestDeleteRequestValidator $
--             mkDeleteRequestValidator
--
--         , requestGetSdkTypes $
--             mkGetSdkTypes
--
--         , requestGetClientCertificates $
--             mkGetClientCertificates
--
--         , requestGetModelTemplate $
--             mkGetModelTemplate
--
--         , requestUpdateDocumentationVersion $
--             mkUpdateDocumentationVersion
--
--         , requestDeleteDocumentationVersion $
--             mkDeleteDocumentationVersion
--
--         , requestGetBasePathMappings $
--             mkGetBasePathMappings
--
--         , requestGetApiKey $
--             mkGetApiKey
--
--           ]

--     , testGroup "response"
--         [ responseGetResource $
--             mkResource
--
--         , responseGetDeployments $
--             mkGetDeploymentsResponse
--
--         , responseGetDeployment $
--             mkDeployment
--
--         , responseGetTags $
--             mkGetTagsResponse
--
--         , responseDeleteGatewayResponse $
--             mkDeleteGatewayResponseResponse
--
--         , responseUpdateGatewayResponse $
--             mkGatewayResponse
--
--         , responseCreateUsagePlan $
--             mkUsagePlan
--
--         , responseGetDomainNames $
--             mkGetDomainNamesResponse
--
--         , responseGetClientCertificate $
--             mkClientCertificate
--
--         , responsePutGatewayResponse $
--             mkGatewayResponse
--
--         , responseGetSdkType $
--             mkSdkType
--
--         , responseGetMethodResponse $
--             mkMethodResponse
--
--         , responseGetModels $
--             mkGetModelsResponse
--
--         , responseGetBasePathMapping $
--             mkBasePathMapping
--
--         , responseGetRequestValidators $
--             mkGetRequestValidatorsResponse
--
--         , responsePutMethodResponse $
--             mkMethodResponse
--
--         , responseImportRestApi $
--             mkRestApi
--
--         , responseDeleteMethodResponse $
--             mkDeleteMethodResponseResponse
--
--         , responseUpdateMethodResponse $
--             mkMethodResponse
--
--         , responseDeleteStage $
--             mkDeleteStageResponse
--
--         , responseUpdateStage $
--             mkStage
--
--         , responseGetRestApis $
--             mkGetRestApisResponse
--
--         , responseGetDocumentationVersions $
--             mkGetDocumentationVersionsResponse
--
--         , responseCreateDeployment $
--             mkDeployment
--
--         , responseGetVpcLinks $
--             mkGetVpcLinksResponse
--
--         , responseCreateBasePathMapping $
--             mkBasePathMapping
--
--         , responseGetIntegration $
--             mkIntegration
--
--         , responseGetDocumentationParts $
--             mkGetDocumentationPartsResponse
--
--         , responseUpdateAccount $
--             mkAccount
--
--         , responseGetUsagePlan $
--             mkUsagePlan
--
--         , responseDeleteDeployment $
--             mkDeleteDeploymentResponse
--
--         , responseUpdateDeployment $
--             mkDeployment
--
--         , responseGetDocumentationPart $
--             mkDocumentationPart
--
--         , responseDeleteResource $
--             mkDeleteResourceResponse
--
--         , responseUpdateResource $
--             mkResource
--
--         , responseCreateRequestValidator $
--             mkRequestValidator
--
--         , responseImportDocumentationParts $
--             mkImportDocumentationPartsResponse
--
--         , responseGetUsage $
--             mkUsage
--
--         , responseGetVpcLink $
--             mkVpcLink
--
--         , responseCreateModel $
--             mkModel
--
--         , responseGetIntegrationResponse $
--             mkIntegrationResponse
--
--         , responseCreateDomainName $
--             mkDomainName
--
--         , responseFlushStageAuthorizersCache $
--             mkFlushStageAuthorizersCacheResponse
--
--         , responseGetGatewayResponses $
--             mkGetGatewayResponsesResponse
--
--         , responseDeleteModel $
--             mkDeleteModelResponse
--
--         , responseUpdateModel $
--             mkModel
--
--         , responseGetDocumentationVersion $
--             mkDocumentationVersion
--
--         , responseDeleteApiKey $
--             mkDeleteApiKeyResponse
--
--         , responseUpdateApiKey $
--             mkApiKey
--
--         , responseGetRestApi $
--             mkRestApi
--
--         , responseGetStages $
--             mkGetStagesResponse
--
--         , responsePutRestApi $
--             mkRestApi
--
--         , responseGetMethod $
--             mkMethod
--
--         , responseGetModel $
--             mkModel
--
--         , responseUpdateRestApi $
--             mkRestApi
--
--         , responseDeleteRestApi $
--             mkDeleteRestApiResponse
--
--         , responseImportApiKeys $
--             mkImportApiKeysResponse
--
--         , responseCreateDocumentationPart $
--             mkDocumentationPart
--
--         , responseTestInvokeMethod $
--             mkTestInvokeMethodResponse
--
--         , responseGetRequestValidator $
--             mkRequestValidator
--
--         , responseGetDomainName $
--             mkDomainName
--
--         , responseCreateVpcLink $
--             mkVpcLink
--
--         , responseDeleteDocumentationPart $
--             mkDeleteDocumentationPartResponse
--
--         , responseUpdateDocumentationPart $
--             mkDocumentationPart
--
--         , responseGetAuthorizers $
--             mkGetAuthorizersResponse
--
--         , responseCreateDocumentationVersion $
--             mkDocumentationVersion
--
--         , responsePutIntegrationResponse $
--             mkIntegrationResponse
--
--         , responseGetUsagePlanKeys $
--             mkGetUsagePlanKeysResponse
--
--         , responseDeleteVpcLink $
--             mkDeleteVpcLinkResponse
--
--         , responseUpdateVpcLink $
--             mkVpcLink
--
--         , responseFlushStageCache $
--             mkFlushStageCacheResponse
--
--         , responseCreateRestApi $
--             mkRestApi
--
--         , responseDeleteIntegrationResponse $
--             mkDeleteIntegrationResponseResponse
--
--         , responseUpdateIntegrationResponse $
--             mkIntegrationResponse
--
--         , responseUpdateUsage $
--             mkUsage
--
--         , responseDeleteIntegration $
--             mkDeleteIntegrationResponse'
--
--         , responseUpdateIntegration $
--             mkIntegration
--
--         , responseTestInvokeAuthorizer $
--             mkTestInvokeAuthorizerResponse
--
--         , responseGenerateClientCertificate $
--             mkClientCertificate
--
--         , responseGetResources $
--             mkGetResourcesResponse
--
--         , responseGetUsagePlanKey $
--             mkUsagePlanKey
--
--         , responseGetAccount $
--             mkAccount
--
--         , responsePutIntegration $
--             mkIntegration
--
--         , responseGetAuthorizer $
--             mkAuthorizer
--
--         , responseDeleteUsagePlan $
--             mkDeleteUsagePlanResponse
--
--         , responseUpdateUsagePlan $
--             mkUsagePlan
--
--         , responseGetStage $
--             mkStage
--
--         , responseGetExport $
--             mkGetExportResponse
--
--         , responseGetSdk $
--             mkGetSdkResponse
--
--         , responseGetApiKeys $
--             mkGetApiKeysResponse
--
--         , responseDeleteBasePathMapping $
--             mkDeleteBasePathMappingResponse
--
--         , responseUpdateBasePathMapping $
--             mkBasePathMapping
--
--         , responseDeleteClientCertificate $
--             mkDeleteClientCertificateResponse
--
--         , responseUpdateClientCertificate $
--             mkClientCertificate
--
--         , responseGetGatewayResponse $
--             mkGatewayResponse
--
--         , responseCreateUsagePlanKey $
--             mkUsagePlanKey
--
--         , responseCreateAuthorizer $
--             mkAuthorizer
--
--         , responseUpdateAuthorizer $
--             mkAuthorizer
--
--         , responseDeleteAuthorizer $
--             mkDeleteAuthorizerResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseCreateStage $
--             mkStage
--
--         , responseDeleteUsagePlanKey $
--             mkDeleteUsagePlanKeyResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseCreateApiKey $
--             mkApiKey
--
--         , responseGetUsagePlans $
--             mkGetUsagePlansResponse
--
--         , responsePutMethod $
--             mkMethod
--
--         , responseUpdateDomainName $
--             mkDomainName
--
--         , responseDeleteDomainName $
--             mkDeleteDomainNameResponse
--
--         , responseCreateResource $
--             mkResource
--
--         , responseDeleteMethod $
--             mkDeleteMethodResponse'
--
--         , responseUpdateMethod $
--             mkMethod
--
--         , responseUpdateRequestValidator $
--             mkRequestValidator
--
--         , responseDeleteRequestValidator $
--             mkDeleteRequestValidatorResponse
--
--         , responseGetSdkTypes $
--             mkGetSdkTypesResponse
--
--         , responseGetClientCertificates $
--             mkGetClientCertificatesResponse
--
--         , responseGetModelTemplate $
--             mkGetModelTemplateResponse
--
--         , responseUpdateDocumentationVersion $
--             mkDocumentationVersion
--
--         , responseDeleteDocumentationVersion $
--             mkDeleteDocumentationVersionResponse
--
--         , responseGetBasePathMappings $
--             mkGetBasePathMappingsResponse
--
--         , responseGetApiKey $
--             mkApiKey
--
--           ]
--     ]

-- Requests

requestGetResource :: GetResource -> TestTree
requestGetResource = req
    "GetResource"
    "fixture/GetResource.yaml"

requestGetDeployments :: GetDeployments -> TestTree
requestGetDeployments = req
    "GetDeployments"
    "fixture/GetDeployments.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment = req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags = req
    "GetTags"
    "fixture/GetTags.yaml"

requestDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
requestDeleteGatewayResponse = req
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.yaml"

requestUpdateGatewayResponse :: UpdateGatewayResponse -> TestTree
requestUpdateGatewayResponse = req
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.yaml"

requestCreateUsagePlan :: CreateUsagePlan -> TestTree
requestCreateUsagePlan = req
    "CreateUsagePlan"
    "fixture/CreateUsagePlan.yaml"

requestGetDomainNames :: GetDomainNames -> TestTree
requestGetDomainNames = req
    "GetDomainNames"
    "fixture/GetDomainNames.yaml"

requestGetClientCertificate :: GetClientCertificate -> TestTree
requestGetClientCertificate = req
    "GetClientCertificate"
    "fixture/GetClientCertificate.yaml"

requestPutGatewayResponse :: PutGatewayResponse -> TestTree
requestPutGatewayResponse = req
    "PutGatewayResponse"
    "fixture/PutGatewayResponse.yaml"

requestGetSdkType :: GetSdkType -> TestTree
requestGetSdkType = req
    "GetSdkType"
    "fixture/GetSdkType.yaml"

requestGetMethodResponse :: GetMethodResponse -> TestTree
requestGetMethodResponse = req
    "GetMethodResponse"
    "fixture/GetMethodResponse.yaml"

requestGetModels :: GetModels -> TestTree
requestGetModels = req
    "GetModels"
    "fixture/GetModels.yaml"

requestGetBasePathMapping :: GetBasePathMapping -> TestTree
requestGetBasePathMapping = req
    "GetBasePathMapping"
    "fixture/GetBasePathMapping.yaml"

requestGetRequestValidators :: GetRequestValidators -> TestTree
requestGetRequestValidators = req
    "GetRequestValidators"
    "fixture/GetRequestValidators.yaml"

requestPutMethodResponse :: PutMethodResponse -> TestTree
requestPutMethodResponse = req
    "PutMethodResponse"
    "fixture/PutMethodResponse.yaml"

requestImportRestApi :: ImportRestApi -> TestTree
requestImportRestApi = req
    "ImportRestApi"
    "fixture/ImportRestApi.yaml"

requestDeleteMethodResponse :: DeleteMethodResponse -> TestTree
requestDeleteMethodResponse = req
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.yaml"

requestUpdateMethodResponse :: UpdateMethodResponse -> TestTree
requestUpdateMethodResponse = req
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.yaml"

requestDeleteStage :: DeleteStage -> TestTree
requestDeleteStage = req
    "DeleteStage"
    "fixture/DeleteStage.yaml"

requestUpdateStage :: UpdateStage -> TestTree
requestUpdateStage = req
    "UpdateStage"
    "fixture/UpdateStage.yaml"

requestGetRestApis :: GetRestApis -> TestTree
requestGetRestApis = req
    "GetRestApis"
    "fixture/GetRestApis.yaml"

requestGetDocumentationVersions :: GetDocumentationVersions -> TestTree
requestGetDocumentationVersions = req
    "GetDocumentationVersions"
    "fixture/GetDocumentationVersions.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment = req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestGetVpcLinks :: GetVpcLinks -> TestTree
requestGetVpcLinks = req
    "GetVpcLinks"
    "fixture/GetVpcLinks.yaml"

requestCreateBasePathMapping :: CreateBasePathMapping -> TestTree
requestCreateBasePathMapping = req
    "CreateBasePathMapping"
    "fixture/CreateBasePathMapping.yaml"

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration = req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

requestGetDocumentationParts :: GetDocumentationParts -> TestTree
requestGetDocumentationParts = req
    "GetDocumentationParts"
    "fixture/GetDocumentationParts.yaml"

requestUpdateAccount :: UpdateAccount -> TestTree
requestUpdateAccount = req
    "UpdateAccount"
    "fixture/UpdateAccount.yaml"

requestGetUsagePlan :: GetUsagePlan -> TestTree
requestGetUsagePlan = req
    "GetUsagePlan"
    "fixture/GetUsagePlan.yaml"

requestDeleteDeployment :: DeleteDeployment -> TestTree
requestDeleteDeployment = req
    "DeleteDeployment"
    "fixture/DeleteDeployment.yaml"

requestUpdateDeployment :: UpdateDeployment -> TestTree
requestUpdateDeployment = req
    "UpdateDeployment"
    "fixture/UpdateDeployment.yaml"

requestGetDocumentationPart :: GetDocumentationPart -> TestTree
requestGetDocumentationPart = req
    "GetDocumentationPart"
    "fixture/GetDocumentationPart.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource = req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource = req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestCreateRequestValidator :: CreateRequestValidator -> TestTree
requestCreateRequestValidator = req
    "CreateRequestValidator"
    "fixture/CreateRequestValidator.yaml"

requestImportDocumentationParts :: ImportDocumentationParts -> TestTree
requestImportDocumentationParts = req
    "ImportDocumentationParts"
    "fixture/ImportDocumentationParts.yaml"

requestGetUsage :: GetUsage -> TestTree
requestGetUsage = req
    "GetUsage"
    "fixture/GetUsage.yaml"

requestGetVpcLink :: GetVpcLink -> TestTree
requestGetVpcLink = req
    "GetVpcLink"
    "fixture/GetVpcLink.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel = req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestGetIntegrationResponse :: GetIntegrationResponse -> TestTree
requestGetIntegrationResponse = req
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.yaml"

requestCreateDomainName :: CreateDomainName -> TestTree
requestCreateDomainName = req
    "CreateDomainName"
    "fixture/CreateDomainName.yaml"

requestFlushStageAuthorizersCache :: FlushStageAuthorizersCache -> TestTree
requestFlushStageAuthorizersCache = req
    "FlushStageAuthorizersCache"
    "fixture/FlushStageAuthorizersCache.yaml"

requestGetGatewayResponses :: GetGatewayResponses -> TestTree
requestGetGatewayResponses = req
    "GetGatewayResponses"
    "fixture/GetGatewayResponses.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel = req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel = req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestGetDocumentationVersion :: GetDocumentationVersion -> TestTree
requestGetDocumentationVersion = req
    "GetDocumentationVersion"
    "fixture/GetDocumentationVersion.yaml"

requestDeleteApiKey :: DeleteApiKey -> TestTree
requestDeleteApiKey = req
    "DeleteApiKey"
    "fixture/DeleteApiKey.yaml"

requestUpdateApiKey :: UpdateApiKey -> TestTree
requestUpdateApiKey = req
    "UpdateApiKey"
    "fixture/UpdateApiKey.yaml"

requestGetRestApi :: GetRestApi -> TestTree
requestGetRestApi = req
    "GetRestApi"
    "fixture/GetRestApi.yaml"

requestGetStages :: GetStages -> TestTree
requestGetStages = req
    "GetStages"
    "fixture/GetStages.yaml"

requestPutRestApi :: PutRestApi -> TestTree
requestPutRestApi = req
    "PutRestApi"
    "fixture/PutRestApi.yaml"

requestGetMethod :: GetMethod -> TestTree
requestGetMethod = req
    "GetMethod"
    "fixture/GetMethod.yaml"

requestGetModel :: GetModel -> TestTree
requestGetModel = req
    "GetModel"
    "fixture/GetModel.yaml"

requestUpdateRestApi :: UpdateRestApi -> TestTree
requestUpdateRestApi = req
    "UpdateRestApi"
    "fixture/UpdateRestApi.yaml"

requestDeleteRestApi :: DeleteRestApi -> TestTree
requestDeleteRestApi = req
    "DeleteRestApi"
    "fixture/DeleteRestApi.yaml"

requestImportApiKeys :: ImportApiKeys -> TestTree
requestImportApiKeys = req
    "ImportApiKeys"
    "fixture/ImportApiKeys.yaml"

requestCreateDocumentationPart :: CreateDocumentationPart -> TestTree
requestCreateDocumentationPart = req
    "CreateDocumentationPart"
    "fixture/CreateDocumentationPart.yaml"

requestTestInvokeMethod :: TestInvokeMethod -> TestTree
requestTestInvokeMethod = req
    "TestInvokeMethod"
    "fixture/TestInvokeMethod.yaml"

requestGetRequestValidator :: GetRequestValidator -> TestTree
requestGetRequestValidator = req
    "GetRequestValidator"
    "fixture/GetRequestValidator.yaml"

requestGetDomainName :: GetDomainName -> TestTree
requestGetDomainName = req
    "GetDomainName"
    "fixture/GetDomainName.yaml"

requestCreateVpcLink :: CreateVpcLink -> TestTree
requestCreateVpcLink = req
    "CreateVpcLink"
    "fixture/CreateVpcLink.yaml"

requestDeleteDocumentationPart :: DeleteDocumentationPart -> TestTree
requestDeleteDocumentationPart = req
    "DeleteDocumentationPart"
    "fixture/DeleteDocumentationPart.yaml"

requestUpdateDocumentationPart :: UpdateDocumentationPart -> TestTree
requestUpdateDocumentationPart = req
    "UpdateDocumentationPart"
    "fixture/UpdateDocumentationPart.yaml"

requestGetAuthorizers :: GetAuthorizers -> TestTree
requestGetAuthorizers = req
    "GetAuthorizers"
    "fixture/GetAuthorizers.yaml"

requestCreateDocumentationVersion :: CreateDocumentationVersion -> TestTree
requestCreateDocumentationVersion = req
    "CreateDocumentationVersion"
    "fixture/CreateDocumentationVersion.yaml"

requestPutIntegrationResponse :: PutIntegrationResponse -> TestTree
requestPutIntegrationResponse = req
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.yaml"

requestGetUsagePlanKeys :: GetUsagePlanKeys -> TestTree
requestGetUsagePlanKeys = req
    "GetUsagePlanKeys"
    "fixture/GetUsagePlanKeys.yaml"

requestDeleteVpcLink :: DeleteVpcLink -> TestTree
requestDeleteVpcLink = req
    "DeleteVpcLink"
    "fixture/DeleteVpcLink.yaml"

requestUpdateVpcLink :: UpdateVpcLink -> TestTree
requestUpdateVpcLink = req
    "UpdateVpcLink"
    "fixture/UpdateVpcLink.yaml"

requestFlushStageCache :: FlushStageCache -> TestTree
requestFlushStageCache = req
    "FlushStageCache"
    "fixture/FlushStageCache.yaml"

requestCreateRestApi :: CreateRestApi -> TestTree
requestCreateRestApi = req
    "CreateRestApi"
    "fixture/CreateRestApi.yaml"

requestDeleteIntegrationResponse :: DeleteIntegrationResponse -> TestTree
requestDeleteIntegrationResponse = req
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.yaml"

requestUpdateIntegrationResponse :: UpdateIntegrationResponse -> TestTree
requestUpdateIntegrationResponse = req
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.yaml"

requestUpdateUsage :: UpdateUsage -> TestTree
requestUpdateUsage = req
    "UpdateUsage"
    "fixture/UpdateUsage.yaml"

requestDeleteIntegration :: DeleteIntegration -> TestTree
requestDeleteIntegration = req
    "DeleteIntegration"
    "fixture/DeleteIntegration.yaml"

requestUpdateIntegration :: UpdateIntegration -> TestTree
requestUpdateIntegration = req
    "UpdateIntegration"
    "fixture/UpdateIntegration.yaml"

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer = req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

requestGenerateClientCertificate :: GenerateClientCertificate -> TestTree
requestGenerateClientCertificate = req
    "GenerateClientCertificate"
    "fixture/GenerateClientCertificate.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources = req
    "GetResources"
    "fixture/GetResources.yaml"

requestGetUsagePlanKey :: GetUsagePlanKey -> TestTree
requestGetUsagePlanKey = req
    "GetUsagePlanKey"
    "fixture/GetUsagePlanKey.yaml"

requestGetAccount :: GetAccount -> TestTree
requestGetAccount = req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestPutIntegration :: PutIntegration -> TestTree
requestPutIntegration = req
    "PutIntegration"
    "fixture/PutIntegration.yaml"

requestGetAuthorizer :: GetAuthorizer -> TestTree
requestGetAuthorizer = req
    "GetAuthorizer"
    "fixture/GetAuthorizer.yaml"

requestDeleteUsagePlan :: DeleteUsagePlan -> TestTree
requestDeleteUsagePlan = req
    "DeleteUsagePlan"
    "fixture/DeleteUsagePlan.yaml"

requestUpdateUsagePlan :: UpdateUsagePlan -> TestTree
requestUpdateUsagePlan = req
    "UpdateUsagePlan"
    "fixture/UpdateUsagePlan.yaml"

requestGetStage :: GetStage -> TestTree
requestGetStage = req
    "GetStage"
    "fixture/GetStage.yaml"

requestGetExport :: GetExport -> TestTree
requestGetExport = req
    "GetExport"
    "fixture/GetExport.yaml"

requestGetSdk :: GetSdk -> TestTree
requestGetSdk = req
    "GetSdk"
    "fixture/GetSdk.yaml"

requestGetApiKeys :: GetApiKeys -> TestTree
requestGetApiKeys = req
    "GetApiKeys"
    "fixture/GetApiKeys.yaml"

requestDeleteBasePathMapping :: DeleteBasePathMapping -> TestTree
requestDeleteBasePathMapping = req
    "DeleteBasePathMapping"
    "fixture/DeleteBasePathMapping.yaml"

requestUpdateBasePathMapping :: UpdateBasePathMapping -> TestTree
requestUpdateBasePathMapping = req
    "UpdateBasePathMapping"
    "fixture/UpdateBasePathMapping.yaml"

requestDeleteClientCertificate :: DeleteClientCertificate -> TestTree
requestDeleteClientCertificate = req
    "DeleteClientCertificate"
    "fixture/DeleteClientCertificate.yaml"

requestUpdateClientCertificate :: UpdateClientCertificate -> TestTree
requestUpdateClientCertificate = req
    "UpdateClientCertificate"
    "fixture/UpdateClientCertificate.yaml"

requestGetGatewayResponse :: GetGatewayResponse -> TestTree
requestGetGatewayResponse = req
    "GetGatewayResponse"
    "fixture/GetGatewayResponse.yaml"

requestCreateUsagePlanKey :: CreateUsagePlanKey -> TestTree
requestCreateUsagePlanKey = req
    "CreateUsagePlanKey"
    "fixture/CreateUsagePlanKey.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer = req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer = req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer = req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateStage :: CreateStage -> TestTree
requestCreateStage = req
    "CreateStage"
    "fixture/CreateStage.yaml"

requestDeleteUsagePlanKey :: DeleteUsagePlanKey -> TestTree
requestDeleteUsagePlanKey = req
    "DeleteUsagePlanKey"
    "fixture/DeleteUsagePlanKey.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateApiKey :: CreateApiKey -> TestTree
requestCreateApiKey = req
    "CreateApiKey"
    "fixture/CreateApiKey.yaml"

requestGetUsagePlans :: GetUsagePlans -> TestTree
requestGetUsagePlans = req
    "GetUsagePlans"
    "fixture/GetUsagePlans.yaml"

requestPutMethod :: PutMethod -> TestTree
requestPutMethod = req
    "PutMethod"
    "fixture/PutMethod.yaml"

requestUpdateDomainName :: UpdateDomainName -> TestTree
requestUpdateDomainName = req
    "UpdateDomainName"
    "fixture/UpdateDomainName.yaml"

requestDeleteDomainName :: DeleteDomainName -> TestTree
requestDeleteDomainName = req
    "DeleteDomainName"
    "fixture/DeleteDomainName.yaml"

requestCreateResource :: CreateResource -> TestTree
requestCreateResource = req
    "CreateResource"
    "fixture/CreateResource.yaml"

requestDeleteMethod :: DeleteMethod -> TestTree
requestDeleteMethod = req
    "DeleteMethod"
    "fixture/DeleteMethod.yaml"

requestUpdateMethod :: UpdateMethod -> TestTree
requestUpdateMethod = req
    "UpdateMethod"
    "fixture/UpdateMethod.yaml"

requestUpdateRequestValidator :: UpdateRequestValidator -> TestTree
requestUpdateRequestValidator = req
    "UpdateRequestValidator"
    "fixture/UpdateRequestValidator.yaml"

requestDeleteRequestValidator :: DeleteRequestValidator -> TestTree
requestDeleteRequestValidator = req
    "DeleteRequestValidator"
    "fixture/DeleteRequestValidator.yaml"

requestGetSdkTypes :: GetSdkTypes -> TestTree
requestGetSdkTypes = req
    "GetSdkTypes"
    "fixture/GetSdkTypes.yaml"

requestGetClientCertificates :: GetClientCertificates -> TestTree
requestGetClientCertificates = req
    "GetClientCertificates"
    "fixture/GetClientCertificates.yaml"

requestGetModelTemplate :: GetModelTemplate -> TestTree
requestGetModelTemplate = req
    "GetModelTemplate"
    "fixture/GetModelTemplate.yaml"

requestUpdateDocumentationVersion :: UpdateDocumentationVersion -> TestTree
requestUpdateDocumentationVersion = req
    "UpdateDocumentationVersion"
    "fixture/UpdateDocumentationVersion.yaml"

requestDeleteDocumentationVersion :: DeleteDocumentationVersion -> TestTree
requestDeleteDocumentationVersion = req
    "DeleteDocumentationVersion"
    "fixture/DeleteDocumentationVersion.yaml"

requestGetBasePathMappings :: GetBasePathMappings -> TestTree
requestGetBasePathMappings = req
    "GetBasePathMappings"
    "fixture/GetBasePathMappings.yaml"

requestGetApiKey :: GetApiKey -> TestTree
requestGetApiKey = req
    "GetApiKey"
    "fixture/GetApiKey.yaml"

-- Responses

responseGetResource :: Resource -> TestTree
responseGetResource = res
    "GetResourceResponse"
    "fixture/GetResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetResource)

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments = res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDeployments)

responseGetDeployment :: Deployment -> TestTree
responseGetDeployment = res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDeployment)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags = res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTags)

responseDeleteGatewayResponse :: DeleteGatewayResponseResponse -> TestTree
responseDeleteGatewayResponse = res
    "DeleteGatewayResponseResponse"
    "fixture/DeleteGatewayResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGatewayResponse)

responseUpdateGatewayResponse :: GatewayResponse -> TestTree
responseUpdateGatewayResponse = res
    "UpdateGatewayResponseResponse"
    "fixture/UpdateGatewayResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGatewayResponse)

responseCreateUsagePlan :: UsagePlan -> TestTree
responseCreateUsagePlan = res
    "CreateUsagePlanResponse"
    "fixture/CreateUsagePlanResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUsagePlan)

responseGetDomainNames :: GetDomainNamesResponse -> TestTree
responseGetDomainNames = res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDomainNames)

responseGetClientCertificate :: ClientCertificate -> TestTree
responseGetClientCertificate = res
    "GetClientCertificateResponse"
    "fixture/GetClientCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetClientCertificate)

responsePutGatewayResponse :: GatewayResponse -> TestTree
responsePutGatewayResponse = res
    "PutGatewayResponseResponse"
    "fixture/PutGatewayResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutGatewayResponse)

responseGetSdkType :: SdkType -> TestTree
responseGetSdkType = res
    "GetSdkTypeResponse"
    "fixture/GetSdkTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSdkType)

responseGetMethodResponse :: MethodResponse -> TestTree
responseGetMethodResponse = res
    "GetMethodResponseResponse"
    "fixture/GetMethodResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetMethodResponse)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels = res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetModels)

responseGetBasePathMapping :: BasePathMapping -> TestTree
responseGetBasePathMapping = res
    "GetBasePathMappingResponse"
    "fixture/GetBasePathMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBasePathMapping)

responseGetRequestValidators :: GetRequestValidatorsResponse -> TestTree
responseGetRequestValidators = res
    "GetRequestValidatorsResponse"
    "fixture/GetRequestValidatorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRequestValidators)

responsePutMethodResponse :: MethodResponse -> TestTree
responsePutMethodResponse = res
    "PutMethodResponseResponse"
    "fixture/PutMethodResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutMethodResponse)

responseImportRestApi :: RestApi -> TestTree
responseImportRestApi = res
    "ImportRestApiResponse"
    "fixture/ImportRestApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportRestApi)

responseDeleteMethodResponse :: DeleteMethodResponseResponse -> TestTree
responseDeleteMethodResponse = res
    "DeleteMethodResponseResponse"
    "fixture/DeleteMethodResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMethodResponse)

responseUpdateMethodResponse :: MethodResponse -> TestTree
responseUpdateMethodResponse = res
    "UpdateMethodResponseResponse"
    "fixture/UpdateMethodResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateMethodResponse)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage = res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStage)

responseUpdateStage :: Stage -> TestTree
responseUpdateStage = res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateStage)

responseGetRestApis :: GetRestApisResponse -> TestTree
responseGetRestApis = res
    "GetRestApisResponse"
    "fixture/GetRestApisResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRestApis)

responseGetDocumentationVersions :: GetDocumentationVersionsResponse -> TestTree
responseGetDocumentationVersions = res
    "GetDocumentationVersionsResponse"
    "fixture/GetDocumentationVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDocumentationVersions)

responseCreateDeployment :: Deployment -> TestTree
responseCreateDeployment = res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDeployment)

responseGetVpcLinks :: GetVpcLinksResponse -> TestTree
responseGetVpcLinks = res
    "GetVpcLinksResponse"
    "fixture/GetVpcLinksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetVpcLinks)

responseCreateBasePathMapping :: BasePathMapping -> TestTree
responseCreateBasePathMapping = res
    "CreateBasePathMappingResponse"
    "fixture/CreateBasePathMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBasePathMapping)

responseGetIntegration :: Integration -> TestTree
responseGetIntegration = res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIntegration)

responseGetDocumentationParts :: GetDocumentationPartsResponse -> TestTree
responseGetDocumentationParts = res
    "GetDocumentationPartsResponse"
    "fixture/GetDocumentationPartsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDocumentationParts)

responseUpdateAccount :: Account -> TestTree
responseUpdateAccount = res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAccount)

responseGetUsagePlan :: UsagePlan -> TestTree
responseGetUsagePlan = res
    "GetUsagePlanResponse"
    "fixture/GetUsagePlanResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUsagePlan)

responseDeleteDeployment :: DeleteDeploymentResponse -> TestTree
responseDeleteDeployment = res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDeployment)

responseUpdateDeployment :: Deployment -> TestTree
responseUpdateDeployment = res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDeployment)

responseGetDocumentationPart :: DocumentationPart -> TestTree
responseGetDocumentationPart = res
    "GetDocumentationPartResponse"
    "fixture/GetDocumentationPartResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDocumentationPart)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource = res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteResource)

responseUpdateResource :: Resource -> TestTree
responseUpdateResource = res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateResource)

responseCreateRequestValidator :: RequestValidator -> TestTree
responseCreateRequestValidator = res
    "CreateRequestValidatorResponse"
    "fixture/CreateRequestValidatorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRequestValidator)

responseImportDocumentationParts :: ImportDocumentationPartsResponse -> TestTree
responseImportDocumentationParts = res
    "ImportDocumentationPartsResponse"
    "fixture/ImportDocumentationPartsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportDocumentationParts)

responseGetUsage :: Usage -> TestTree
responseGetUsage = res
    "GetUsageResponse"
    "fixture/GetUsageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUsage)

responseGetVpcLink :: VpcLink -> TestTree
responseGetVpcLink = res
    "GetVpcLinkResponse"
    "fixture/GetVpcLinkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetVpcLink)

responseCreateModel :: Model -> TestTree
responseCreateModel = res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateModel)

responseGetIntegrationResponse :: IntegrationResponse -> TestTree
responseGetIntegrationResponse = res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIntegrationResponse)

responseCreateDomainName :: DomainName -> TestTree
responseCreateDomainName = res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDomainName)

responseFlushStageAuthorizersCache :: FlushStageAuthorizersCacheResponse -> TestTree
responseFlushStageAuthorizersCache = res
    "FlushStageAuthorizersCacheResponse"
    "fixture/FlushStageAuthorizersCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy FlushStageAuthorizersCache)

responseGetGatewayResponses :: GetGatewayResponsesResponse -> TestTree
responseGetGatewayResponses = res
    "GetGatewayResponsesResponse"
    "fixture/GetGatewayResponsesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGatewayResponses)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel = res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteModel)

responseUpdateModel :: Model -> TestTree
responseUpdateModel = res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateModel)

responseGetDocumentationVersion :: DocumentationVersion -> TestTree
responseGetDocumentationVersion = res
    "GetDocumentationVersionResponse"
    "fixture/GetDocumentationVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDocumentationVersion)

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey = res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApiKey)

responseUpdateApiKey :: ApiKey -> TestTree
responseUpdateApiKey = res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApiKey)

responseGetRestApi :: RestApi -> TestTree
responseGetRestApi = res
    "GetRestApiResponse"
    "fixture/GetRestApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRestApi)

responseGetStages :: GetStagesResponse -> TestTree
responseGetStages = res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetStages)

responsePutRestApi :: RestApi -> TestTree
responsePutRestApi = res
    "PutRestApiResponse"
    "fixture/PutRestApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutRestApi)

responseGetMethod :: Method -> TestTree
responseGetMethod = res
    "GetMethodResponse"
    "fixture/GetMethodResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetMethod)

responseGetModel :: Model -> TestTree
responseGetModel = res
    "GetModelResponse"
    "fixture/GetModelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetModel)

responseUpdateRestApi :: RestApi -> TestTree
responseUpdateRestApi = res
    "UpdateRestApiResponse"
    "fixture/UpdateRestApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRestApi)

responseDeleteRestApi :: DeleteRestApiResponse -> TestTree
responseDeleteRestApi = res
    "DeleteRestApiResponse"
    "fixture/DeleteRestApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRestApi)

responseImportApiKeys :: ImportApiKeysResponse -> TestTree
responseImportApiKeys = res
    "ImportApiKeysResponse"
    "fixture/ImportApiKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportApiKeys)

responseCreateDocumentationPart :: DocumentationPart -> TestTree
responseCreateDocumentationPart = res
    "CreateDocumentationPartResponse"
    "fixture/CreateDocumentationPartResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDocumentationPart)

responseTestInvokeMethod :: TestInvokeMethodResponse -> TestTree
responseTestInvokeMethod = res
    "TestInvokeMethodResponse"
    "fixture/TestInvokeMethodResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestInvokeMethod)

responseGetRequestValidator :: RequestValidator -> TestTree
responseGetRequestValidator = res
    "GetRequestValidatorResponse"
    "fixture/GetRequestValidatorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRequestValidator)

responseGetDomainName :: DomainName -> TestTree
responseGetDomainName = res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDomainName)

responseCreateVpcLink :: VpcLink -> TestTree
responseCreateVpcLink = res
    "CreateVpcLinkResponse"
    "fixture/CreateVpcLinkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVpcLink)

responseDeleteDocumentationPart :: DeleteDocumentationPartResponse -> TestTree
responseDeleteDocumentationPart = res
    "DeleteDocumentationPartResponse"
    "fixture/DeleteDocumentationPartResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDocumentationPart)

responseUpdateDocumentationPart :: DocumentationPart -> TestTree
responseUpdateDocumentationPart = res
    "UpdateDocumentationPartResponse"
    "fixture/UpdateDocumentationPartResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDocumentationPart)

responseGetAuthorizers :: GetAuthorizersResponse -> TestTree
responseGetAuthorizers = res
    "GetAuthorizersResponse"
    "fixture/GetAuthorizersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAuthorizers)

responseCreateDocumentationVersion :: DocumentationVersion -> TestTree
responseCreateDocumentationVersion = res
    "CreateDocumentationVersionResponse"
    "fixture/CreateDocumentationVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDocumentationVersion)

responsePutIntegrationResponse :: IntegrationResponse -> TestTree
responsePutIntegrationResponse = res
    "PutIntegrationResponseResponse"
    "fixture/PutIntegrationResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutIntegrationResponse)

responseGetUsagePlanKeys :: GetUsagePlanKeysResponse -> TestTree
responseGetUsagePlanKeys = res
    "GetUsagePlanKeysResponse"
    "fixture/GetUsagePlanKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUsagePlanKeys)

responseDeleteVpcLink :: DeleteVpcLinkResponse -> TestTree
responseDeleteVpcLink = res
    "DeleteVpcLinkResponse"
    "fixture/DeleteVpcLinkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVpcLink)

responseUpdateVpcLink :: VpcLink -> TestTree
responseUpdateVpcLink = res
    "UpdateVpcLinkResponse"
    "fixture/UpdateVpcLinkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateVpcLink)

responseFlushStageCache :: FlushStageCacheResponse -> TestTree
responseFlushStageCache = res
    "FlushStageCacheResponse"
    "fixture/FlushStageCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy FlushStageCache)

responseCreateRestApi :: RestApi -> TestTree
responseCreateRestApi = res
    "CreateRestApiResponse"
    "fixture/CreateRestApiResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRestApi)

responseDeleteIntegrationResponse :: DeleteIntegrationResponseResponse -> TestTree
responseDeleteIntegrationResponse = res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteIntegrationResponse)

responseUpdateIntegrationResponse :: IntegrationResponse -> TestTree
responseUpdateIntegrationResponse = res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateIntegrationResponse)

responseUpdateUsage :: Usage -> TestTree
responseUpdateUsage = res
    "UpdateUsageResponse"
    "fixture/UpdateUsageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUsage)

responseDeleteIntegration :: DeleteIntegrationResponse' -> TestTree
responseDeleteIntegration = res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteIntegration)

responseUpdateIntegration :: Integration -> TestTree
responseUpdateIntegration = res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateIntegration)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer = res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestInvokeAuthorizer)

responseGenerateClientCertificate :: ClientCertificate -> TestTree
responseGenerateClientCertificate = res
    "GenerateClientCertificateResponse"
    "fixture/GenerateClientCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateClientCertificate)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources = res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetResources)

responseGetUsagePlanKey :: UsagePlanKey -> TestTree
responseGetUsagePlanKey = res
    "GetUsagePlanKeyResponse"
    "fixture/GetUsagePlanKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUsagePlanKey)

responseGetAccount :: Account -> TestTree
responseGetAccount = res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAccount)

responsePutIntegration :: Integration -> TestTree
responsePutIntegration = res
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutIntegration)

responseGetAuthorizer :: Authorizer -> TestTree
responseGetAuthorizer = res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAuthorizer)

responseDeleteUsagePlan :: DeleteUsagePlanResponse -> TestTree
responseDeleteUsagePlan = res
    "DeleteUsagePlanResponse"
    "fixture/DeleteUsagePlanResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUsagePlan)

responseUpdateUsagePlan :: UsagePlan -> TestTree
responseUpdateUsagePlan = res
    "UpdateUsagePlanResponse"
    "fixture/UpdateUsagePlanResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateUsagePlan)

responseGetStage :: Stage -> TestTree
responseGetStage = res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetStage)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport = res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetExport)

responseGetSdk :: GetSdkResponse -> TestTree
responseGetSdk = res
    "GetSdkResponse"
    "fixture/GetSdkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSdk)

responseGetApiKeys :: GetApiKeysResponse -> TestTree
responseGetApiKeys = res
    "GetApiKeysResponse"
    "fixture/GetApiKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApiKeys)

responseDeleteBasePathMapping :: DeleteBasePathMappingResponse -> TestTree
responseDeleteBasePathMapping = res
    "DeleteBasePathMappingResponse"
    "fixture/DeleteBasePathMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBasePathMapping)

responseUpdateBasePathMapping :: BasePathMapping -> TestTree
responseUpdateBasePathMapping = res
    "UpdateBasePathMappingResponse"
    "fixture/UpdateBasePathMappingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateBasePathMapping)

responseDeleteClientCertificate :: DeleteClientCertificateResponse -> TestTree
responseDeleteClientCertificate = res
    "DeleteClientCertificateResponse"
    "fixture/DeleteClientCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteClientCertificate)

responseUpdateClientCertificate :: ClientCertificate -> TestTree
responseUpdateClientCertificate = res
    "UpdateClientCertificateResponse"
    "fixture/UpdateClientCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateClientCertificate)

responseGetGatewayResponse :: GatewayResponse -> TestTree
responseGetGatewayResponse = res
    "GetGatewayResponseResponse"
    "fixture/GetGatewayResponseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGatewayResponse)

responseCreateUsagePlanKey :: UsagePlanKey -> TestTree
responseCreateUsagePlanKey = res
    "CreateUsagePlanKeyResponse"
    "fixture/CreateUsagePlanKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUsagePlanKey)

responseCreateAuthorizer :: Authorizer -> TestTree
responseCreateAuthorizer = res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAuthorizer)

responseUpdateAuthorizer :: Authorizer -> TestTree
responseUpdateAuthorizer = res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAuthorizer)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer = res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAuthorizer)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseCreateStage :: Stage -> TestTree
responseCreateStage = res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStage)

responseDeleteUsagePlanKey :: DeleteUsagePlanKeyResponse -> TestTree
responseDeleteUsagePlanKey = res
    "DeleteUsagePlanKeyResponse"
    "fixture/DeleteUsagePlanKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUsagePlanKey)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseCreateApiKey :: ApiKey -> TestTree
responseCreateApiKey = res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateApiKey)

responseGetUsagePlans :: GetUsagePlansResponse -> TestTree
responseGetUsagePlans = res
    "GetUsagePlansResponse"
    "fixture/GetUsagePlansResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUsagePlans)

responsePutMethod :: Method -> TestTree
responsePutMethod = res
    "PutMethodResponse"
    "fixture/PutMethodResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutMethod)

responseUpdateDomainName :: DomainName -> TestTree
responseUpdateDomainName = res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDomainName)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName = res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDomainName)

responseCreateResource :: Resource -> TestTree
responseCreateResource = res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateResource)

responseDeleteMethod :: DeleteMethodResponse' -> TestTree
responseDeleteMethod = res
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteMethod)

responseUpdateMethod :: Method -> TestTree
responseUpdateMethod = res
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateMethod)

responseUpdateRequestValidator :: RequestValidator -> TestTree
responseUpdateRequestValidator = res
    "UpdateRequestValidatorResponse"
    "fixture/UpdateRequestValidatorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRequestValidator)

responseDeleteRequestValidator :: DeleteRequestValidatorResponse -> TestTree
responseDeleteRequestValidator = res
    "DeleteRequestValidatorResponse"
    "fixture/DeleteRequestValidatorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRequestValidator)

responseGetSdkTypes :: GetSdkTypesResponse -> TestTree
responseGetSdkTypes = res
    "GetSdkTypesResponse"
    "fixture/GetSdkTypesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSdkTypes)

responseGetClientCertificates :: GetClientCertificatesResponse -> TestTree
responseGetClientCertificates = res
    "GetClientCertificatesResponse"
    "fixture/GetClientCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetClientCertificates)

responseGetModelTemplate :: GetModelTemplateResponse -> TestTree
responseGetModelTemplate = res
    "GetModelTemplateResponse"
    "fixture/GetModelTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetModelTemplate)

responseUpdateDocumentationVersion :: DocumentationVersion -> TestTree
responseUpdateDocumentationVersion = res
    "UpdateDocumentationVersionResponse"
    "fixture/UpdateDocumentationVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDocumentationVersion)

responseDeleteDocumentationVersion :: DeleteDocumentationVersionResponse -> TestTree
responseDeleteDocumentationVersion = res
    "DeleteDocumentationVersionResponse"
    "fixture/DeleteDocumentationVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDocumentationVersion)

responseGetBasePathMappings :: GetBasePathMappingsResponse -> TestTree
responseGetBasePathMappings = res
    "GetBasePathMappingsResponse"
    "fixture/GetBasePathMappingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBasePathMappings)

responseGetApiKey :: ApiKey -> TestTree
responseGetApiKey = res
    "GetApiKeyResponse"
    "fixture/GetApiKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApiKey)
