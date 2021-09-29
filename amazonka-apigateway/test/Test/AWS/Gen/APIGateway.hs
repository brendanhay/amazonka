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

import Data.Proxy
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
--         [ requestUpdateIntegration $
--             newUpdateIntegration
--
--         , requestUpdateUsagePlan $
--             newUpdateUsagePlan
--
--         , requestGetAuthorizer $
--             newGetAuthorizer
--
--         , requestDeleteUsagePlan $
--             newDeleteUsagePlan
--
--         , requestDeleteIntegration $
--             newDeleteIntegration
--
--         , requestGenerateClientCertificate $
--             newGenerateClientCertificate
--
--         , requestGetUsagePlanKey $
--             newGetUsagePlanKey
--
--         , requestCreateRestApi $
--             newCreateRestApi
--
--         , requestDeleteIntegrationResponse $
--             newDeleteIntegrationResponse
--
--         , requestUpdateDocumentationPart $
--             newUpdateDocumentationPart
--
--         , requestUpdateIntegrationResponse $
--             newUpdateIntegrationResponse
--
--         , requestGetDeployments $
--             newGetDeployments
--
--         , requestDeleteDocumentationPart $
--             newDeleteDocumentationPart
--
--         , requestUpdateUsage $
--             newUpdateUsage
--
--         , requestUpdateVpcLink $
--             newUpdateVpcLink
--
--         , requestDeleteVpcLink $
--             newDeleteVpcLink
--
--         , requestFlushStageCache $
--             newFlushStageCache
--
--         , requestGetModel $
--             newGetModel
--
--         , requestGetClientCertificates $
--             newGetClientCertificates
--
--         , requestPutRestApi $
--             newPutRestApi
--
--         , requestTestInvokeMethod $
--             newTestInvokeMethod
--
--         , requestGetBasePathMappings $
--             newGetBasePathMappings
--
--         , requestGetApiKey $
--             newGetApiKey
--
--         , requestGetSdkTypes $
--             newGetSdkTypes
--
--         , requestUpdateRequestValidator $
--             newUpdateRequestValidator
--
--         , requestGetGatewayResponses $
--             newGetGatewayResponses
--
--         , requestUpdateModel $
--             newUpdateModel
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestDeleteDomainName $
--             newDeleteDomainName
--
--         , requestDeleteMethod $
--             newDeleteMethod
--
--         , requestCreateResource $
--             newCreateResource
--
--         , requestUpdateDomainName $
--             newUpdateDomainName
--
--         , requestDeleteRequestValidator $
--             newDeleteRequestValidator
--
--         , requestGetDocumentationVersion $
--             newGetDocumentationVersion
--
--         , requestUpdateMethod $
--             newUpdateMethod
--
--         , requestCreateDomainName $
--             newCreateDomainName
--
--         , requestImportDocumentationParts $
--             newImportDocumentationParts
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestGetDocumentationPart $
--             newGetDocumentationPart
--
--         , requestGetUsagePlans $
--             newGetUsagePlans
--
--         , requestGetUsage $
--             newGetUsage
--
--         , requestGetVpcLink $
--             newGetVpcLink
--
--         , requestGetIntegrationResponse $
--             newGetIntegrationResponse
--
--         , requestCreateRequestValidator $
--             newCreateRequestValidator
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetIntegration $
--             newGetIntegration
--
--         , requestUpdateDeployment $
--             newUpdateDeployment
--
--         , requestGetUsagePlan $
--             newGetUsagePlan
--
--         , requestDeleteDeployment $
--             newDeleteDeployment
--
--         , requestCreateStage $
--             newCreateStage
--
--         , requestCreateUsagePlanKey $
--             newCreateUsagePlanKey
--
--         , requestPutMethodResponse $
--             newPutMethodResponse
--
--         , requestDeleteStage $
--             newDeleteStage
--
--         , requestCreateAuthorizer $
--             newCreateAuthorizer
--
--         , requestUpdateStage $
--             newUpdateStage
--
--         , requestImportRestApi $
--             newImportRestApi
--
--         , requestGetRestApis $
--             newGetRestApis
--
--         , requestGetGatewayResponse $
--             newGetGatewayResponse
--
--         , requestGetRequestValidators $
--             newGetRequestValidators
--
--         , requestGetExport $
--             newGetExport
--
--         , requestGetSdkType $
--             newGetSdkType
--
--         , requestPutGatewayResponse $
--             newPutGatewayResponse
--
--         , requestGetBasePathMapping $
--             newGetBasePathMapping
--
--         , requestGetDomainNames $
--             newGetDomainNames
--
--         , requestGetClientCertificate $
--             newGetClientCertificate
--
--         , requestPutIntegration $
--             newPutIntegration
--
--         , requestGetResources $
--             newGetResources
--
--         , requestGetTags $
--             newGetTags
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestGetAccount $
--             newGetAccount
--
--         , requestTestInvokeAuthorizer $
--             newTestInvokeAuthorizer
--
--         , requestPutIntegrationResponse $
--             newPutIntegrationResponse
--
--         , requestGetResource $
--             newGetResource
--
--         , requestCreateDocumentationVersion $
--             newCreateDocumentationVersion
--
--         , requestGetAuthorizers $
--             newGetAuthorizers
--
--         , requestGetUsagePlanKeys $
--             newGetUsagePlanKeys
--
--         , requestCreateVpcLink $
--             newCreateVpcLink
--
--         , requestGetDomainName $
--             newGetDomainName
--
--         , requestGetModelTemplate $
--             newGetModelTemplate
--
--         , requestUpdateDocumentationVersion $
--             newUpdateDocumentationVersion
--
--         , requestDeleteRestApi $
--             newDeleteRestApi
--
--         , requestGetRequestValidator $
--             newGetRequestValidator
--
--         , requestCreateDocumentationPart $
--             newCreateDocumentationPart
--
--         , requestDeleteDocumentationVersion $
--             newDeleteDocumentationVersion
--
--         , requestImportApiKeys $
--             newImportApiKeys
--
--         , requestUpdateRestApi $
--             newUpdateRestApi
--
--         , requestGetStages $
--             newGetStages
--
--         , requestGetMethod $
--             newGetMethod
--
--         , requestFlushStageAuthorizersCache $
--             newFlushStageAuthorizersCache
--
--         , requestDeleteApiKey $
--             newDeleteApiKey
--
--         , requestGetRestApi $
--             newGetRestApi
--
--         , requestUpdateApiKey $
--             newUpdateApiKey
--
--         , requestPutMethod $
--             newPutMethod
--
--         , requestCreateApiKey $
--             newCreateApiKey
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestUpdateAccount $
--             newUpdateAccount
--
--         , requestGetDocumentationParts $
--             newGetDocumentationParts
--
--         , requestUpdateAuthorizer $
--             newUpdateAuthorizer
--
--         , requestGetVpcLinks $
--             newGetVpcLinks
--
--         , requestDeleteUsagePlanKey $
--             newDeleteUsagePlanKey
--
--         , requestDeleteAuthorizer $
--             newDeleteAuthorizer
--
--         , requestCreateBasePathMapping $
--             newCreateBasePathMapping
--
--         , requestUpdateBasePathMapping $
--             newUpdateBasePathMapping
--
--         , requestGetDocumentationVersions $
--             newGetDocumentationVersions
--
--         , requestDeleteMethodResponse $
--             newDeleteMethodResponse
--
--         , requestUpdateClientCertificate $
--             newUpdateClientCertificate
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestUpdateMethodResponse $
--             newUpdateMethodResponse
--
--         , requestDeleteBasePathMapping $
--             newDeleteBasePathMapping
--
--         , requestDeleteClientCertificate $
--             newDeleteClientCertificate
--
--         , requestDeleteGatewayResponse $
--             newDeleteGatewayResponse
--
--         , requestGetModels $
--             newGetModels
--
--         , requestCreateUsagePlan $
--             newCreateUsagePlan
--
--         , requestUpdateGatewayResponse $
--             newUpdateGatewayResponse
--
--         , requestGetSdk $
--             newGetSdk
--
--         , requestGetMethodResponse $
--             newGetMethodResponse
--
--         , requestGetApiKeys $
--             newGetApiKeys
--
--         , requestGetStage $
--             newGetStage
--
--           ]

--     , testGroup "response"
--         [ responseUpdateIntegration $
--             newIntegration
--
--         , responseUpdateUsagePlan $
--             newUsagePlan
--
--         , responseGetAuthorizer $
--             newAuthorizer
--
--         , responseDeleteUsagePlan $
--             newDeleteUsagePlanResponse
--
--         , responseDeleteIntegration $
--             newDeleteIntegrationResponse'
--
--         , responseGenerateClientCertificate $
--             newClientCertificate
--
--         , responseGetUsagePlanKey $
--             newUsagePlanKey
--
--         , responseCreateRestApi $
--             newRestApi
--
--         , responseDeleteIntegrationResponse $
--             newDeleteIntegrationResponseResponse
--
--         , responseUpdateDocumentationPart $
--             newDocumentationPart
--
--         , responseUpdateIntegrationResponse $
--             newIntegrationResponse
--
--         , responseGetDeployments $
--             newGetDeploymentsResponse
--
--         , responseDeleteDocumentationPart $
--             newDeleteDocumentationPartResponse
--
--         , responseUpdateUsage $
--             newUsage
--
--         , responseUpdateVpcLink $
--             newVpcLink
--
--         , responseDeleteVpcLink $
--             newDeleteVpcLinkResponse
--
--         , responseFlushStageCache $
--             newFlushStageCacheResponse
--
--         , responseGetModel $
--             newModel
--
--         , responseGetClientCertificates $
--             newGetClientCertificatesResponse
--
--         , responsePutRestApi $
--             newRestApi
--
--         , responseTestInvokeMethod $
--             newTestInvokeMethodResponse
--
--         , responseGetBasePathMappings $
--             newGetBasePathMappingsResponse
--
--         , responseGetApiKey $
--             newApiKey
--
--         , responseGetSdkTypes $
--             newGetSdkTypesResponse
--
--         , responseUpdateRequestValidator $
--             newRequestValidator
--
--         , responseGetGatewayResponses $
--             newGetGatewayResponsesResponse
--
--         , responseUpdateModel $
--             newModel
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseDeleteDomainName $
--             newDeleteDomainNameResponse
--
--         , responseDeleteMethod $
--             newDeleteMethodResponse'
--
--         , responseCreateResource $
--             newResource
--
--         , responseUpdateDomainName $
--             newDomainName
--
--         , responseDeleteRequestValidator $
--             newDeleteRequestValidatorResponse
--
--         , responseGetDocumentationVersion $
--             newDocumentationVersion
--
--         , responseUpdateMethod $
--             newMethod
--
--         , responseCreateDomainName $
--             newDomainName
--
--         , responseImportDocumentationParts $
--             newImportDocumentationPartsResponse
--
--         , responseUpdateResource $
--             newResource
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseGetDocumentationPart $
--             newDocumentationPart
--
--         , responseGetUsagePlans $
--             newGetUsagePlansResponse
--
--         , responseGetUsage $
--             newUsage
--
--         , responseGetVpcLink $
--             newVpcLink
--
--         , responseGetIntegrationResponse $
--             newIntegrationResponse
--
--         , responseCreateRequestValidator $
--             newRequestValidator
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetIntegration $
--             newIntegration
--
--         , responseUpdateDeployment $
--             newDeployment
--
--         , responseGetUsagePlan $
--             newUsagePlan
--
--         , responseDeleteDeployment $
--             newDeleteDeploymentResponse
--
--         , responseCreateStage $
--             newStage
--
--         , responseCreateUsagePlanKey $
--             newUsagePlanKey
--
--         , responsePutMethodResponse $
--             newMethodResponse
--
--         , responseDeleteStage $
--             newDeleteStageResponse
--
--         , responseCreateAuthorizer $
--             newAuthorizer
--
--         , responseUpdateStage $
--             newStage
--
--         , responseImportRestApi $
--             newRestApi
--
--         , responseGetRestApis $
--             newGetRestApisResponse
--
--         , responseGetGatewayResponse $
--             newGatewayResponse
--
--         , responseGetRequestValidators $
--             newGetRequestValidatorsResponse
--
--         , responseGetExport $
--             newGetExportResponse
--
--         , responseGetSdkType $
--             newSdkType
--
--         , responsePutGatewayResponse $
--             newGatewayResponse
--
--         , responseGetBasePathMapping $
--             newBasePathMapping
--
--         , responseGetDomainNames $
--             newGetDomainNamesResponse
--
--         , responseGetClientCertificate $
--             newClientCertificate
--
--         , responsePutIntegration $
--             newIntegration
--
--         , responseGetResources $
--             newGetResourcesResponse
--
--         , responseGetTags $
--             newGetTagsResponse
--
--         , responseGetDeployment $
--             newDeployment
--
--         , responseGetAccount $
--             newAccount
--
--         , responseTestInvokeAuthorizer $
--             newTestInvokeAuthorizerResponse
--
--         , responsePutIntegrationResponse $
--             newIntegrationResponse
--
--         , responseGetResource $
--             newResource
--
--         , responseCreateDocumentationVersion $
--             newDocumentationVersion
--
--         , responseGetAuthorizers $
--             newGetAuthorizersResponse
--
--         , responseGetUsagePlanKeys $
--             newGetUsagePlanKeysResponse
--
--         , responseCreateVpcLink $
--             newVpcLink
--
--         , responseGetDomainName $
--             newDomainName
--
--         , responseGetModelTemplate $
--             newGetModelTemplateResponse
--
--         , responseUpdateDocumentationVersion $
--             newDocumentationVersion
--
--         , responseDeleteRestApi $
--             newDeleteRestApiResponse
--
--         , responseGetRequestValidator $
--             newRequestValidator
--
--         , responseCreateDocumentationPart $
--             newDocumentationPart
--
--         , responseDeleteDocumentationVersion $
--             newDeleteDocumentationVersionResponse
--
--         , responseImportApiKeys $
--             newImportApiKeysResponse
--
--         , responseUpdateRestApi $
--             newRestApi
--
--         , responseGetStages $
--             newGetStagesResponse
--
--         , responseGetMethod $
--             newMethod
--
--         , responseFlushStageAuthorizersCache $
--             newFlushStageAuthorizersCacheResponse
--
--         , responseDeleteApiKey $
--             newDeleteApiKeyResponse
--
--         , responseGetRestApi $
--             newRestApi
--
--         , responseUpdateApiKey $
--             newApiKey
--
--         , responsePutMethod $
--             newMethod
--
--         , responseCreateApiKey $
--             newApiKey
--
--         , responseCreateModel $
--             newModel
--
--         , responseUpdateAccount $
--             newAccount
--
--         , responseGetDocumentationParts $
--             newGetDocumentationPartsResponse
--
--         , responseUpdateAuthorizer $
--             newAuthorizer
--
--         , responseGetVpcLinks $
--             newGetVpcLinksResponse
--
--         , responseDeleteUsagePlanKey $
--             newDeleteUsagePlanKeyResponse
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseCreateBasePathMapping $
--             newBasePathMapping
--
--         , responseUpdateBasePathMapping $
--             newBasePathMapping
--
--         , responseGetDocumentationVersions $
--             newGetDocumentationVersionsResponse
--
--         , responseDeleteMethodResponse $
--             newDeleteMethodResponseResponse
--
--         , responseUpdateClientCertificate $
--             newClientCertificate
--
--         , responseCreateDeployment $
--             newDeployment
--
--         , responseUpdateMethodResponse $
--             newMethodResponse
--
--         , responseDeleteBasePathMapping $
--             newDeleteBasePathMappingResponse
--
--         , responseDeleteClientCertificate $
--             newDeleteClientCertificateResponse
--
--         , responseDeleteGatewayResponse $
--             newDeleteGatewayResponseResponse
--
--         , responseGetModels $
--             newGetModelsResponse
--
--         , responseCreateUsagePlan $
--             newUsagePlan
--
--         , responseUpdateGatewayResponse $
--             newGatewayResponse
--
--         , responseGetSdk $
--             newGetSdkResponse
--
--         , responseGetMethodResponse $
--             newMethodResponse
--
--         , responseGetApiKeys $
--             newGetApiKeysResponse
--
--         , responseGetStage $
--             newStage
--
--           ]
--     ]

-- Requests

requestUpdateIntegration :: UpdateIntegration -> TestTree
requestUpdateIntegration =
  req
    "UpdateIntegration"
    "fixture/UpdateIntegration.yaml"

requestUpdateUsagePlan :: UpdateUsagePlan -> TestTree
requestUpdateUsagePlan =
  req
    "UpdateUsagePlan"
    "fixture/UpdateUsagePlan.yaml"

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

requestDeleteIntegration :: DeleteIntegration -> TestTree
requestDeleteIntegration =
  req
    "DeleteIntegration"
    "fixture/DeleteIntegration.yaml"

requestGenerateClientCertificate :: GenerateClientCertificate -> TestTree
requestGenerateClientCertificate =
  req
    "GenerateClientCertificate"
    "fixture/GenerateClientCertificate.yaml"

requestGetUsagePlanKey :: GetUsagePlanKey -> TestTree
requestGetUsagePlanKey =
  req
    "GetUsagePlanKey"
    "fixture/GetUsagePlanKey.yaml"

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

requestUpdateDocumentationPart :: UpdateDocumentationPart -> TestTree
requestUpdateDocumentationPart =
  req
    "UpdateDocumentationPart"
    "fixture/UpdateDocumentationPart.yaml"

requestUpdateIntegrationResponse :: UpdateIntegrationResponse -> TestTree
requestUpdateIntegrationResponse =
  req
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.yaml"

requestGetDeployments :: GetDeployments -> TestTree
requestGetDeployments =
  req
    "GetDeployments"
    "fixture/GetDeployments.yaml"

requestDeleteDocumentationPart :: DeleteDocumentationPart -> TestTree
requestDeleteDocumentationPart =
  req
    "DeleteDocumentationPart"
    "fixture/DeleteDocumentationPart.yaml"

requestUpdateUsage :: UpdateUsage -> TestTree
requestUpdateUsage =
  req
    "UpdateUsage"
    "fixture/UpdateUsage.yaml"

requestUpdateVpcLink :: UpdateVpcLink -> TestTree
requestUpdateVpcLink =
  req
    "UpdateVpcLink"
    "fixture/UpdateVpcLink.yaml"

requestDeleteVpcLink :: DeleteVpcLink -> TestTree
requestDeleteVpcLink =
  req
    "DeleteVpcLink"
    "fixture/DeleteVpcLink.yaml"

requestFlushStageCache :: FlushStageCache -> TestTree
requestFlushStageCache =
  req
    "FlushStageCache"
    "fixture/FlushStageCache.yaml"

requestGetModel :: GetModel -> TestTree
requestGetModel =
  req
    "GetModel"
    "fixture/GetModel.yaml"

requestGetClientCertificates :: GetClientCertificates -> TestTree
requestGetClientCertificates =
  req
    "GetClientCertificates"
    "fixture/GetClientCertificates.yaml"

requestPutRestApi :: PutRestApi -> TestTree
requestPutRestApi =
  req
    "PutRestApi"
    "fixture/PutRestApi.yaml"

requestTestInvokeMethod :: TestInvokeMethod -> TestTree
requestTestInvokeMethod =
  req
    "TestInvokeMethod"
    "fixture/TestInvokeMethod.yaml"

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

requestGetSdkTypes :: GetSdkTypes -> TestTree
requestGetSdkTypes =
  req
    "GetSdkTypes"
    "fixture/GetSdkTypes.yaml"

requestUpdateRequestValidator :: UpdateRequestValidator -> TestTree
requestUpdateRequestValidator =
  req
    "UpdateRequestValidator"
    "fixture/UpdateRequestValidator.yaml"

requestGetGatewayResponses :: GetGatewayResponses -> TestTree
requestGetGatewayResponses =
  req
    "GetGatewayResponses"
    "fixture/GetGatewayResponses.yaml"

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel =
  req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestDeleteDomainName :: DeleteDomainName -> TestTree
requestDeleteDomainName =
  req
    "DeleteDomainName"
    "fixture/DeleteDomainName.yaml"

requestDeleteMethod :: DeleteMethod -> TestTree
requestDeleteMethod =
  req
    "DeleteMethod"
    "fixture/DeleteMethod.yaml"

requestCreateResource :: CreateResource -> TestTree
requestCreateResource =
  req
    "CreateResource"
    "fixture/CreateResource.yaml"

requestUpdateDomainName :: UpdateDomainName -> TestTree
requestUpdateDomainName =
  req
    "UpdateDomainName"
    "fixture/UpdateDomainName.yaml"

requestDeleteRequestValidator :: DeleteRequestValidator -> TestTree
requestDeleteRequestValidator =
  req
    "DeleteRequestValidator"
    "fixture/DeleteRequestValidator.yaml"

requestGetDocumentationVersion :: GetDocumentationVersion -> TestTree
requestGetDocumentationVersion =
  req
    "GetDocumentationVersion"
    "fixture/GetDocumentationVersion.yaml"

requestUpdateMethod :: UpdateMethod -> TestTree
requestUpdateMethod =
  req
    "UpdateMethod"
    "fixture/UpdateMethod.yaml"

requestCreateDomainName :: CreateDomainName -> TestTree
requestCreateDomainName =
  req
    "CreateDomainName"
    "fixture/CreateDomainName.yaml"

requestImportDocumentationParts :: ImportDocumentationParts -> TestTree
requestImportDocumentationParts =
  req
    "ImportDocumentationParts"
    "fixture/ImportDocumentationParts.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestGetDocumentationPart :: GetDocumentationPart -> TestTree
requestGetDocumentationPart =
  req
    "GetDocumentationPart"
    "fixture/GetDocumentationPart.yaml"

requestGetUsagePlans :: GetUsagePlans -> TestTree
requestGetUsagePlans =
  req
    "GetUsagePlans"
    "fixture/GetUsagePlans.yaml"

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

requestGetIntegrationResponse :: GetIntegrationResponse -> TestTree
requestGetIntegrationResponse =
  req
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.yaml"

requestCreateRequestValidator :: CreateRequestValidator -> TestTree
requestCreateRequestValidator =
  req
    "CreateRequestValidator"
    "fixture/CreateRequestValidator.yaml"

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

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration =
  req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

requestUpdateDeployment :: UpdateDeployment -> TestTree
requestUpdateDeployment =
  req
    "UpdateDeployment"
    "fixture/UpdateDeployment.yaml"

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

requestCreateStage :: CreateStage -> TestTree
requestCreateStage =
  req
    "CreateStage"
    "fixture/CreateStage.yaml"

requestCreateUsagePlanKey :: CreateUsagePlanKey -> TestTree
requestCreateUsagePlanKey =
  req
    "CreateUsagePlanKey"
    "fixture/CreateUsagePlanKey.yaml"

requestPutMethodResponse :: PutMethodResponse -> TestTree
requestPutMethodResponse =
  req
    "PutMethodResponse"
    "fixture/PutMethodResponse.yaml"

requestDeleteStage :: DeleteStage -> TestTree
requestDeleteStage =
  req
    "DeleteStage"
    "fixture/DeleteStage.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer =
  req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

requestUpdateStage :: UpdateStage -> TestTree
requestUpdateStage =
  req
    "UpdateStage"
    "fixture/UpdateStage.yaml"

requestImportRestApi :: ImportRestApi -> TestTree
requestImportRestApi =
  req
    "ImportRestApi"
    "fixture/ImportRestApi.yaml"

requestGetRestApis :: GetRestApis -> TestTree
requestGetRestApis =
  req
    "GetRestApis"
    "fixture/GetRestApis.yaml"

requestGetGatewayResponse :: GetGatewayResponse -> TestTree
requestGetGatewayResponse =
  req
    "GetGatewayResponse"
    "fixture/GetGatewayResponse.yaml"

requestGetRequestValidators :: GetRequestValidators -> TestTree
requestGetRequestValidators =
  req
    "GetRequestValidators"
    "fixture/GetRequestValidators.yaml"

requestGetExport :: GetExport -> TestTree
requestGetExport =
  req
    "GetExport"
    "fixture/GetExport.yaml"

requestGetSdkType :: GetSdkType -> TestTree
requestGetSdkType =
  req
    "GetSdkType"
    "fixture/GetSdkType.yaml"

requestPutGatewayResponse :: PutGatewayResponse -> TestTree
requestPutGatewayResponse =
  req
    "PutGatewayResponse"
    "fixture/PutGatewayResponse.yaml"

requestGetBasePathMapping :: GetBasePathMapping -> TestTree
requestGetBasePathMapping =
  req
    "GetBasePathMapping"
    "fixture/GetBasePathMapping.yaml"

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

requestPutIntegration :: PutIntegration -> TestTree
requestPutIntegration =
  req
    "PutIntegration"
    "fixture/PutIntegration.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources =
  req
    "GetResources"
    "fixture/GetResources.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags =
  req
    "GetTags"
    "fixture/GetTags.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestGetAccount :: GetAccount -> TestTree
requestGetAccount =
  req
    "GetAccount"
    "fixture/GetAccount.yaml"

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer =
  req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

requestPutIntegrationResponse :: PutIntegrationResponse -> TestTree
requestPutIntegrationResponse =
  req
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.yaml"

requestGetResource :: GetResource -> TestTree
requestGetResource =
  req
    "GetResource"
    "fixture/GetResource.yaml"

requestCreateDocumentationVersion :: CreateDocumentationVersion -> TestTree
requestCreateDocumentationVersion =
  req
    "CreateDocumentationVersion"
    "fixture/CreateDocumentationVersion.yaml"

requestGetAuthorizers :: GetAuthorizers -> TestTree
requestGetAuthorizers =
  req
    "GetAuthorizers"
    "fixture/GetAuthorizers.yaml"

requestGetUsagePlanKeys :: GetUsagePlanKeys -> TestTree
requestGetUsagePlanKeys =
  req
    "GetUsagePlanKeys"
    "fixture/GetUsagePlanKeys.yaml"

requestCreateVpcLink :: CreateVpcLink -> TestTree
requestCreateVpcLink =
  req
    "CreateVpcLink"
    "fixture/CreateVpcLink.yaml"

requestGetDomainName :: GetDomainName -> TestTree
requestGetDomainName =
  req
    "GetDomainName"
    "fixture/GetDomainName.yaml"

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

requestDeleteRestApi :: DeleteRestApi -> TestTree
requestDeleteRestApi =
  req
    "DeleteRestApi"
    "fixture/DeleteRestApi.yaml"

requestGetRequestValidator :: GetRequestValidator -> TestTree
requestGetRequestValidator =
  req
    "GetRequestValidator"
    "fixture/GetRequestValidator.yaml"

requestCreateDocumentationPart :: CreateDocumentationPart -> TestTree
requestCreateDocumentationPart =
  req
    "CreateDocumentationPart"
    "fixture/CreateDocumentationPart.yaml"

requestDeleteDocumentationVersion :: DeleteDocumentationVersion -> TestTree
requestDeleteDocumentationVersion =
  req
    "DeleteDocumentationVersion"
    "fixture/DeleteDocumentationVersion.yaml"

requestImportApiKeys :: ImportApiKeys -> TestTree
requestImportApiKeys =
  req
    "ImportApiKeys"
    "fixture/ImportApiKeys.yaml"

requestUpdateRestApi :: UpdateRestApi -> TestTree
requestUpdateRestApi =
  req
    "UpdateRestApi"
    "fixture/UpdateRestApi.yaml"

requestGetStages :: GetStages -> TestTree
requestGetStages =
  req
    "GetStages"
    "fixture/GetStages.yaml"

requestGetMethod :: GetMethod -> TestTree
requestGetMethod =
  req
    "GetMethod"
    "fixture/GetMethod.yaml"

requestFlushStageAuthorizersCache :: FlushStageAuthorizersCache -> TestTree
requestFlushStageAuthorizersCache =
  req
    "FlushStageAuthorizersCache"
    "fixture/FlushStageAuthorizersCache.yaml"

requestDeleteApiKey :: DeleteApiKey -> TestTree
requestDeleteApiKey =
  req
    "DeleteApiKey"
    "fixture/DeleteApiKey.yaml"

requestGetRestApi :: GetRestApi -> TestTree
requestGetRestApi =
  req
    "GetRestApi"
    "fixture/GetRestApi.yaml"

requestUpdateApiKey :: UpdateApiKey -> TestTree
requestUpdateApiKey =
  req
    "UpdateApiKey"
    "fixture/UpdateApiKey.yaml"

requestPutMethod :: PutMethod -> TestTree
requestPutMethod =
  req
    "PutMethod"
    "fixture/PutMethod.yaml"

requestCreateApiKey :: CreateApiKey -> TestTree
requestCreateApiKey =
  req
    "CreateApiKey"
    "fixture/CreateApiKey.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestUpdateAccount :: UpdateAccount -> TestTree
requestUpdateAccount =
  req
    "UpdateAccount"
    "fixture/UpdateAccount.yaml"

requestGetDocumentationParts :: GetDocumentationParts -> TestTree
requestGetDocumentationParts =
  req
    "GetDocumentationParts"
    "fixture/GetDocumentationParts.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer =
  req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestGetVpcLinks :: GetVpcLinks -> TestTree
requestGetVpcLinks =
  req
    "GetVpcLinks"
    "fixture/GetVpcLinks.yaml"

requestDeleteUsagePlanKey :: DeleteUsagePlanKey -> TestTree
requestDeleteUsagePlanKey =
  req
    "DeleteUsagePlanKey"
    "fixture/DeleteUsagePlanKey.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer =
  req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestCreateBasePathMapping :: CreateBasePathMapping -> TestTree
requestCreateBasePathMapping =
  req
    "CreateBasePathMapping"
    "fixture/CreateBasePathMapping.yaml"

requestUpdateBasePathMapping :: UpdateBasePathMapping -> TestTree
requestUpdateBasePathMapping =
  req
    "UpdateBasePathMapping"
    "fixture/UpdateBasePathMapping.yaml"

requestGetDocumentationVersions :: GetDocumentationVersions -> TestTree
requestGetDocumentationVersions =
  req
    "GetDocumentationVersions"
    "fixture/GetDocumentationVersions.yaml"

requestDeleteMethodResponse :: DeleteMethodResponse -> TestTree
requestDeleteMethodResponse =
  req
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.yaml"

requestUpdateClientCertificate :: UpdateClientCertificate -> TestTree
requestUpdateClientCertificate =
  req
    "UpdateClientCertificate"
    "fixture/UpdateClientCertificate.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestUpdateMethodResponse :: UpdateMethodResponse -> TestTree
requestUpdateMethodResponse =
  req
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.yaml"

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

requestDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
requestDeleteGatewayResponse =
  req
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.yaml"

requestGetModels :: GetModels -> TestTree
requestGetModels =
  req
    "GetModels"
    "fixture/GetModels.yaml"

requestCreateUsagePlan :: CreateUsagePlan -> TestTree
requestCreateUsagePlan =
  req
    "CreateUsagePlan"
    "fixture/CreateUsagePlan.yaml"

requestUpdateGatewayResponse :: UpdateGatewayResponse -> TestTree
requestUpdateGatewayResponse =
  req
    "UpdateGatewayResponse"
    "fixture/UpdateGatewayResponse.yaml"

requestGetSdk :: GetSdk -> TestTree
requestGetSdk =
  req
    "GetSdk"
    "fixture/GetSdk.yaml"

requestGetMethodResponse :: GetMethodResponse -> TestTree
requestGetMethodResponse =
  req
    "GetMethodResponse"
    "fixture/GetMethodResponse.yaml"

requestGetApiKeys :: GetApiKeys -> TestTree
requestGetApiKeys =
  req
    "GetApiKeys"
    "fixture/GetApiKeys.yaml"

requestGetStage :: GetStage -> TestTree
requestGetStage =
  req
    "GetStage"
    "fixture/GetStage.yaml"

-- Responses

responseUpdateIntegration :: Integration -> TestTree
responseUpdateIntegration =
  res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIntegration)

responseUpdateUsagePlan :: UsagePlan -> TestTree
responseUpdateUsagePlan =
  res
    "UpdateUsagePlanResponse"
    "fixture/UpdateUsagePlanResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUsagePlan)

responseGetAuthorizer :: Authorizer -> TestTree
responseGetAuthorizer =
  res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy GetAuthorizer)

responseDeleteUsagePlan :: DeleteUsagePlanResponse -> TestTree
responseDeleteUsagePlan =
  res
    "DeleteUsagePlanResponse"
    "fixture/DeleteUsagePlanResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUsagePlan)

responseDeleteIntegration :: DeleteIntegrationResponse' -> TestTree
responseDeleteIntegration =
  res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIntegration)

responseGenerateClientCertificate :: ClientCertificate -> TestTree
responseGenerateClientCertificate =
  res
    "GenerateClientCertificateResponse"
    "fixture/GenerateClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateClientCertificate)

responseGetUsagePlanKey :: UsagePlanKey -> TestTree
responseGetUsagePlanKey =
  res
    "GetUsagePlanKeyResponse"
    "fixture/GetUsagePlanKeyResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsagePlanKey)

responseCreateRestApi :: RestApi -> TestTree
responseCreateRestApi =
  res
    "CreateRestApiResponse"
    "fixture/CreateRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRestApi)

responseDeleteIntegrationResponse :: DeleteIntegrationResponseResponse -> TestTree
responseDeleteIntegrationResponse =
  res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIntegrationResponse)

responseUpdateDocumentationPart :: DocumentationPart -> TestTree
responseUpdateDocumentationPart =
  res
    "UpdateDocumentationPartResponse"
    "fixture/UpdateDocumentationPartResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentationPart)

responseUpdateIntegrationResponse :: IntegrationResponse -> TestTree
responseUpdateIntegrationResponse =
  res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIntegrationResponse)

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments =
  res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeployments)

responseDeleteDocumentationPart :: DeleteDocumentationPartResponse -> TestTree
responseDeleteDocumentationPart =
  res
    "DeleteDocumentationPartResponse"
    "fixture/DeleteDocumentationPartResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDocumentationPart)

responseUpdateUsage :: Usage -> TestTree
responseUpdateUsage =
  res
    "UpdateUsageResponse"
    "fixture/UpdateUsageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUsage)

responseUpdateVpcLink :: VpcLink -> TestTree
responseUpdateVpcLink =
  res
    "UpdateVpcLinkResponse"
    "fixture/UpdateVpcLinkResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVpcLink)

responseDeleteVpcLink :: DeleteVpcLinkResponse -> TestTree
responseDeleteVpcLink =
  res
    "DeleteVpcLinkResponse"
    "fixture/DeleteVpcLinkResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcLink)

responseFlushStageCache :: FlushStageCacheResponse -> TestTree
responseFlushStageCache =
  res
    "FlushStageCacheResponse"
    "fixture/FlushStageCacheResponse.proto"
    defaultService
    (Proxy :: Proxy FlushStageCache)

responseGetModel :: Model -> TestTree
responseGetModel =
  res
    "GetModelResponse"
    "fixture/GetModelResponse.proto"
    defaultService
    (Proxy :: Proxy GetModel)

responseGetClientCertificates :: GetClientCertificatesResponse -> TestTree
responseGetClientCertificates =
  res
    "GetClientCertificatesResponse"
    "fixture/GetClientCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy GetClientCertificates)

responsePutRestApi :: RestApi -> TestTree
responsePutRestApi =
  res
    "PutRestApiResponse"
    "fixture/PutRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy PutRestApi)

responseTestInvokeMethod :: TestInvokeMethodResponse -> TestTree
responseTestInvokeMethod =
  res
    "TestInvokeMethodResponse"
    "fixture/TestInvokeMethodResponse.proto"
    defaultService
    (Proxy :: Proxy TestInvokeMethod)

responseGetBasePathMappings :: GetBasePathMappingsResponse -> TestTree
responseGetBasePathMappings =
  res
    "GetBasePathMappingsResponse"
    "fixture/GetBasePathMappingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBasePathMappings)

responseGetApiKey :: ApiKey -> TestTree
responseGetApiKey =
  res
    "GetApiKeyResponse"
    "fixture/GetApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy GetApiKey)

responseGetSdkTypes :: GetSdkTypesResponse -> TestTree
responseGetSdkTypes =
  res
    "GetSdkTypesResponse"
    "fixture/GetSdkTypesResponse.proto"
    defaultService
    (Proxy :: Proxy GetSdkTypes)

responseUpdateRequestValidator :: RequestValidator -> TestTree
responseUpdateRequestValidator =
  res
    "UpdateRequestValidatorResponse"
    "fixture/UpdateRequestValidatorResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRequestValidator)

responseGetGatewayResponses :: GetGatewayResponsesResponse -> TestTree
responseGetGatewayResponses =
  res
    "GetGatewayResponsesResponse"
    "fixture/GetGatewayResponsesResponse.proto"
    defaultService
    (Proxy :: Proxy GetGatewayResponses)

responseUpdateModel :: Model -> TestTree
responseUpdateModel =
  res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateModel)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModel)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName =
  res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomainName)

responseDeleteMethod :: DeleteMethodResponse' -> TestTree
responseDeleteMethod =
  res
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMethod)

responseCreateResource :: Resource -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResource)

responseUpdateDomainName :: DomainName -> TestTree
responseUpdateDomainName =
  res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainName)

responseDeleteRequestValidator :: DeleteRequestValidatorResponse -> TestTree
responseDeleteRequestValidator =
  res
    "DeleteRequestValidatorResponse"
    "fixture/DeleteRequestValidatorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRequestValidator)

responseGetDocumentationVersion :: DocumentationVersion -> TestTree
responseGetDocumentationVersion =
  res
    "GetDocumentationVersionResponse"
    "fixture/GetDocumentationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentationVersion)

responseUpdateMethod :: Method -> TestTree
responseUpdateMethod =
  res
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMethod)

responseCreateDomainName :: DomainName -> TestTree
responseCreateDomainName =
  res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomainName)

responseImportDocumentationParts :: ImportDocumentationPartsResponse -> TestTree
responseImportDocumentationParts =
  res
    "ImportDocumentationPartsResponse"
    "fixture/ImportDocumentationPartsResponse.proto"
    defaultService
    (Proxy :: Proxy ImportDocumentationParts)

responseUpdateResource :: Resource -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResource)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResource)

responseGetDocumentationPart :: DocumentationPart -> TestTree
responseGetDocumentationPart =
  res
    "GetDocumentationPartResponse"
    "fixture/GetDocumentationPartResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentationPart)

responseGetUsagePlans :: GetUsagePlansResponse -> TestTree
responseGetUsagePlans =
  res
    "GetUsagePlansResponse"
    "fixture/GetUsagePlansResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsagePlans)

responseGetUsage :: Usage -> TestTree
responseGetUsage =
  res
    "GetUsageResponse"
    "fixture/GetUsageResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsage)

responseGetVpcLink :: VpcLink -> TestTree
responseGetVpcLink =
  res
    "GetVpcLinkResponse"
    "fixture/GetVpcLinkResponse.proto"
    defaultService
    (Proxy :: Proxy GetVpcLink)

responseGetIntegrationResponse :: IntegrationResponse -> TestTree
responseGetIntegrationResponse =
  res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    defaultService
    (Proxy :: Proxy GetIntegrationResponse)

responseCreateRequestValidator :: RequestValidator -> TestTree
responseCreateRequestValidator =
  res
    "CreateRequestValidatorResponse"
    "fixture/CreateRequestValidatorResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRequestValidator)

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

responseGetIntegration :: Integration -> TestTree
responseGetIntegration =
  res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    defaultService
    (Proxy :: Proxy GetIntegration)

responseUpdateDeployment :: Deployment -> TestTree
responseUpdateDeployment =
  res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeployment)

responseGetUsagePlan :: UsagePlan -> TestTree
responseGetUsagePlan =
  res
    "GetUsagePlanResponse"
    "fixture/GetUsagePlanResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsagePlan)

responseDeleteDeployment :: DeleteDeploymentResponse -> TestTree
responseDeleteDeployment =
  res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeployment)

responseCreateStage :: Stage -> TestTree
responseCreateStage =
  res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStage)

responseCreateUsagePlanKey :: UsagePlanKey -> TestTree
responseCreateUsagePlanKey =
  res
    "CreateUsagePlanKeyResponse"
    "fixture/CreateUsagePlanKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUsagePlanKey)

responsePutMethodResponse :: MethodResponse -> TestTree
responsePutMethodResponse =
  res
    "PutMethodResponseResponse"
    "fixture/PutMethodResponseResponse.proto"
    defaultService
    (Proxy :: Proxy PutMethodResponse)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage =
  res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStage)

responseCreateAuthorizer :: Authorizer -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAuthorizer)

responseUpdateStage :: Stage -> TestTree
responseUpdateStage =
  res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStage)

responseImportRestApi :: RestApi -> TestTree
responseImportRestApi =
  res
    "ImportRestApiResponse"
    "fixture/ImportRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy ImportRestApi)

responseGetRestApis :: GetRestApisResponse -> TestTree
responseGetRestApis =
  res
    "GetRestApisResponse"
    "fixture/GetRestApisResponse.proto"
    defaultService
    (Proxy :: Proxy GetRestApis)

responseGetGatewayResponse :: GatewayResponse -> TestTree
responseGetGatewayResponse =
  res
    "GetGatewayResponseResponse"
    "fixture/GetGatewayResponseResponse.proto"
    defaultService
    (Proxy :: Proxy GetGatewayResponse)

responseGetRequestValidators :: GetRequestValidatorsResponse -> TestTree
responseGetRequestValidators =
  res
    "GetRequestValidatorsResponse"
    "fixture/GetRequestValidatorsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRequestValidators)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport =
  res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    defaultService
    (Proxy :: Proxy GetExport)

responseGetSdkType :: SdkType -> TestTree
responseGetSdkType =
  res
    "GetSdkTypeResponse"
    "fixture/GetSdkTypeResponse.proto"
    defaultService
    (Proxy :: Proxy GetSdkType)

responsePutGatewayResponse :: GatewayResponse -> TestTree
responsePutGatewayResponse =
  res
    "PutGatewayResponseResponse"
    "fixture/PutGatewayResponseResponse.proto"
    defaultService
    (Proxy :: Proxy PutGatewayResponse)

responseGetBasePathMapping :: BasePathMapping -> TestTree
responseGetBasePathMapping =
  res
    "GetBasePathMappingResponse"
    "fixture/GetBasePathMappingResponse.proto"
    defaultService
    (Proxy :: Proxy GetBasePathMapping)

responseGetDomainNames :: GetDomainNamesResponse -> TestTree
responseGetDomainNames =
  res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainNames)

responseGetClientCertificate :: ClientCertificate -> TestTree
responseGetClientCertificate =
  res
    "GetClientCertificateResponse"
    "fixture/GetClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GetClientCertificate)

responsePutIntegration :: Integration -> TestTree
responsePutIntegration =
  res
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.proto"
    defaultService
    (Proxy :: Proxy PutIntegration)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy GetResources)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTags)

responseGetDeployment :: Deployment -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeployment)

responseGetAccount :: Account -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccount)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy TestInvokeAuthorizer)

responsePutIntegrationResponse :: IntegrationResponse -> TestTree
responsePutIntegrationResponse =
  res
    "PutIntegrationResponseResponse"
    "fixture/PutIntegrationResponseResponse.proto"
    defaultService
    (Proxy :: Proxy PutIntegrationResponse)

responseGetResource :: Resource -> TestTree
responseGetResource =
  res
    "GetResourceResponse"
    "fixture/GetResourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetResource)

responseCreateDocumentationVersion :: DocumentationVersion -> TestTree
responseCreateDocumentationVersion =
  res
    "CreateDocumentationVersionResponse"
    "fixture/CreateDocumentationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDocumentationVersion)

responseGetAuthorizers :: GetAuthorizersResponse -> TestTree
responseGetAuthorizers =
  res
    "GetAuthorizersResponse"
    "fixture/GetAuthorizersResponse.proto"
    defaultService
    (Proxy :: Proxy GetAuthorizers)

responseGetUsagePlanKeys :: GetUsagePlanKeysResponse -> TestTree
responseGetUsagePlanKeys =
  res
    "GetUsagePlanKeysResponse"
    "fixture/GetUsagePlanKeysResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsagePlanKeys)

responseCreateVpcLink :: VpcLink -> TestTree
responseCreateVpcLink =
  res
    "CreateVpcLinkResponse"
    "fixture/CreateVpcLinkResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcLink)

responseGetDomainName :: DomainName -> TestTree
responseGetDomainName =
  res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainName)

responseGetModelTemplate :: GetModelTemplateResponse -> TestTree
responseGetModelTemplate =
  res
    "GetModelTemplateResponse"
    "fixture/GetModelTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetModelTemplate)

responseUpdateDocumentationVersion :: DocumentationVersion -> TestTree
responseUpdateDocumentationVersion =
  res
    "UpdateDocumentationVersionResponse"
    "fixture/UpdateDocumentationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentationVersion)

responseDeleteRestApi :: DeleteRestApiResponse -> TestTree
responseDeleteRestApi =
  res
    "DeleteRestApiResponse"
    "fixture/DeleteRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRestApi)

responseGetRequestValidator :: RequestValidator -> TestTree
responseGetRequestValidator =
  res
    "GetRequestValidatorResponse"
    "fixture/GetRequestValidatorResponse.proto"
    defaultService
    (Proxy :: Proxy GetRequestValidator)

responseCreateDocumentationPart :: DocumentationPart -> TestTree
responseCreateDocumentationPart =
  res
    "CreateDocumentationPartResponse"
    "fixture/CreateDocumentationPartResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDocumentationPart)

responseDeleteDocumentationVersion :: DeleteDocumentationVersionResponse -> TestTree
responseDeleteDocumentationVersion =
  res
    "DeleteDocumentationVersionResponse"
    "fixture/DeleteDocumentationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDocumentationVersion)

responseImportApiKeys :: ImportApiKeysResponse -> TestTree
responseImportApiKeys =
  res
    "ImportApiKeysResponse"
    "fixture/ImportApiKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ImportApiKeys)

responseUpdateRestApi :: RestApi -> TestTree
responseUpdateRestApi =
  res
    "UpdateRestApiResponse"
    "fixture/UpdateRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRestApi)

responseGetStages :: GetStagesResponse -> TestTree
responseGetStages =
  res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    defaultService
    (Proxy :: Proxy GetStages)

responseGetMethod :: Method -> TestTree
responseGetMethod =
  res
    "GetMethodResponse"
    "fixture/GetMethodResponse.proto"
    defaultService
    (Proxy :: Proxy GetMethod)

responseFlushStageAuthorizersCache :: FlushStageAuthorizersCacheResponse -> TestTree
responseFlushStageAuthorizersCache =
  res
    "FlushStageAuthorizersCacheResponse"
    "fixture/FlushStageAuthorizersCacheResponse.proto"
    defaultService
    (Proxy :: Proxy FlushStageAuthorizersCache)

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey =
  res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApiKey)

responseGetRestApi :: RestApi -> TestTree
responseGetRestApi =
  res
    "GetRestApiResponse"
    "fixture/GetRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy GetRestApi)

responseUpdateApiKey :: ApiKey -> TestTree
responseUpdateApiKey =
  res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApiKey)

responsePutMethod :: Method -> TestTree
responsePutMethod =
  res
    "PutMethodResponse"
    "fixture/PutMethodResponse.proto"
    defaultService
    (Proxy :: Proxy PutMethod)

responseCreateApiKey :: ApiKey -> TestTree
responseCreateApiKey =
  res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApiKey)

responseCreateModel :: Model -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModel)

responseUpdateAccount :: Account -> TestTree
responseUpdateAccount =
  res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccount)

responseGetDocumentationParts :: GetDocumentationPartsResponse -> TestTree
responseGetDocumentationParts =
  res
    "GetDocumentationPartsResponse"
    "fixture/GetDocumentationPartsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentationParts)

responseUpdateAuthorizer :: Authorizer -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAuthorizer)

responseGetVpcLinks :: GetVpcLinksResponse -> TestTree
responseGetVpcLinks =
  res
    "GetVpcLinksResponse"
    "fixture/GetVpcLinksResponse.proto"
    defaultService
    (Proxy :: Proxy GetVpcLinks)

responseDeleteUsagePlanKey :: DeleteUsagePlanKeyResponse -> TestTree
responseDeleteUsagePlanKey =
  res
    "DeleteUsagePlanKeyResponse"
    "fixture/DeleteUsagePlanKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUsagePlanKey)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAuthorizer)

responseCreateBasePathMapping :: BasePathMapping -> TestTree
responseCreateBasePathMapping =
  res
    "CreateBasePathMappingResponse"
    "fixture/CreateBasePathMappingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBasePathMapping)

responseUpdateBasePathMapping :: BasePathMapping -> TestTree
responseUpdateBasePathMapping =
  res
    "UpdateBasePathMappingResponse"
    "fixture/UpdateBasePathMappingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBasePathMapping)

responseGetDocumentationVersions :: GetDocumentationVersionsResponse -> TestTree
responseGetDocumentationVersions =
  res
    "GetDocumentationVersionsResponse"
    "fixture/GetDocumentationVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentationVersions)

responseDeleteMethodResponse :: DeleteMethodResponseResponse -> TestTree
responseDeleteMethodResponse =
  res
    "DeleteMethodResponseResponse"
    "fixture/DeleteMethodResponseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMethodResponse)

responseUpdateClientCertificate :: ClientCertificate -> TestTree
responseUpdateClientCertificate =
  res
    "UpdateClientCertificateResponse"
    "fixture/UpdateClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateClientCertificate)

responseCreateDeployment :: Deployment -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeployment)

responseUpdateMethodResponse :: MethodResponse -> TestTree
responseUpdateMethodResponse =
  res
    "UpdateMethodResponseResponse"
    "fixture/UpdateMethodResponseResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMethodResponse)

responseDeleteBasePathMapping :: DeleteBasePathMappingResponse -> TestTree
responseDeleteBasePathMapping =
  res
    "DeleteBasePathMappingResponse"
    "fixture/DeleteBasePathMappingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBasePathMapping)

responseDeleteClientCertificate :: DeleteClientCertificateResponse -> TestTree
responseDeleteClientCertificate =
  res
    "DeleteClientCertificateResponse"
    "fixture/DeleteClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClientCertificate)

responseDeleteGatewayResponse :: DeleteGatewayResponseResponse -> TestTree
responseDeleteGatewayResponse =
  res
    "DeleteGatewayResponseResponse"
    "fixture/DeleteGatewayResponseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGatewayResponse)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels =
  res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    defaultService
    (Proxy :: Proxy GetModels)

responseCreateUsagePlan :: UsagePlan -> TestTree
responseCreateUsagePlan =
  res
    "CreateUsagePlanResponse"
    "fixture/CreateUsagePlanResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUsagePlan)

responseUpdateGatewayResponse :: GatewayResponse -> TestTree
responseUpdateGatewayResponse =
  res
    "UpdateGatewayResponseResponse"
    "fixture/UpdateGatewayResponseResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGatewayResponse)

responseGetSdk :: GetSdkResponse -> TestTree
responseGetSdk =
  res
    "GetSdkResponse"
    "fixture/GetSdkResponse.proto"
    defaultService
    (Proxy :: Proxy GetSdk)

responseGetMethodResponse :: MethodResponse -> TestTree
responseGetMethodResponse =
  res
    "GetMethodResponseResponse"
    "fixture/GetMethodResponseResponse.proto"
    defaultService
    (Proxy :: Proxy GetMethodResponse)

responseGetApiKeys :: GetApiKeysResponse -> TestTree
responseGetApiKeys =
  res
    "GetApiKeysResponse"
    "fixture/GetApiKeysResponse.proto"
    defaultService
    (Proxy :: Proxy GetApiKeys)

responseGetStage :: Stage -> TestTree
responseGetStage =
  res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    defaultService
    (Proxy :: Proxy GetStage)
