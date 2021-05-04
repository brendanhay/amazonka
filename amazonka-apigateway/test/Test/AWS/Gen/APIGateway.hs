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
--         [ requestGenerateClientCertificate $
--             newGenerateClientCertificate
--
--         , requestUpdateIntegration $
--             newUpdateIntegration
--
--         , requestDeleteUsagePlan $
--             newDeleteUsagePlan
--
--         , requestDeleteIntegration $
--             newDeleteIntegration
--
--         , requestGetUsagePlanKey $
--             newGetUsagePlanKey
--
--         , requestGetAuthorizer $
--             newGetAuthorizer
--
--         , requestUpdateUsagePlan $
--             newUpdateUsagePlan
--
--         , requestGetDeployments $
--             newGetDeployments
--
--         , requestDeleteIntegrationResponse $
--             newDeleteIntegrationResponse
--
--         , requestFlushStageCache $
--             newFlushStageCache
--
--         , requestDeleteVpcLink $
--             newDeleteVpcLink
--
--         , requestDeleteDocumentationPart $
--             newDeleteDocumentationPart
--
--         , requestUpdateVpcLink $
--             newUpdateVpcLink
--
--         , requestCreateRestApi $
--             newCreateRestApi
--
--         , requestUpdateUsage $
--             newUpdateUsage
--
--         , requestUpdateIntegrationResponse $
--             newUpdateIntegrationResponse
--
--         , requestUpdateDocumentationPart $
--             newUpdateDocumentationPart
--
--         , requestGetSdkTypes $
--             newGetSdkTypes
--
--         , requestGetBasePathMappings $
--             newGetBasePathMappings
--
--         , requestGetModel $
--             newGetModel
--
--         , requestGetClientCertificates $
--             newGetClientCertificates
--
--         , requestTestInvokeMethod $
--             newTestInvokeMethod
--
--         , requestPutRestApi $
--             newPutRestApi
--
--         , requestGetApiKey $
--             newGetApiKey
--
--         , requestGetGatewayResponses $
--             newGetGatewayResponses
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestDeleteMethod $
--             newDeleteMethod
--
--         , requestGetDocumentationVersion $
--             newGetDocumentationVersion
--
--         , requestUpdateMethod $
--             newUpdateMethod
--
--         , requestUpdateModel $
--             newUpdateModel
--
--         , requestUpdateDomainName $
--             newUpdateDomainName
--
--         , requestCreateResource $
--             newCreateResource
--
--         , requestDeleteDomainName $
--             newDeleteDomainName
--
--         , requestUpdateRequestValidator $
--             newUpdateRequestValidator
--
--         , requestDeleteRequestValidator $
--             newDeleteRequestValidator
--
--         , requestGetUsagePlans $
--             newGetUsagePlans
--
--         , requestCreateRequestValidator $
--             newCreateRequestValidator
--
--         , requestCreateDomainName $
--             newCreateDomainName
--
--         , requestGetVpcLink $
--             newGetVpcLink
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestGetDocumentationPart $
--             newGetDocumentationPart
--
--         , requestGetUsage $
--             newGetUsage
--
--         , requestImportDocumentationParts $
--             newImportDocumentationParts
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestGetIntegrationResponse $
--             newGetIntegrationResponse
--
--         , requestDeleteDeployment $
--             newDeleteDeployment
--
--         , requestCreateStage $
--             newCreateStage
--
--         , requestGetIntegration $
--             newGetIntegration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUpdateDeployment $
--             newUpdateDeployment
--
--         , requestGetUsagePlan $
--             newGetUsagePlan
--
--         , requestGetRestApis $
--             newGetRestApis
--
--         , requestCreateAuthorizer $
--             newCreateAuthorizer
--
--         , requestDeleteStage $
--             newDeleteStage
--
--         , requestUpdateStage $
--             newUpdateStage
--
--         , requestCreateUsagePlanKey $
--             newCreateUsagePlanKey
--
--         , requestGetGatewayResponse $
--             newGetGatewayResponse
--
--         , requestImportRestApi $
--             newImportRestApi
--
--         , requestPutMethodResponse $
--             newPutMethodResponse
--
--         , requestGetBasePathMapping $
--             newGetBasePathMapping
--
--         , requestGetRequestValidators $
--             newGetRequestValidators
--
--         , requestGetDomainNames $
--             newGetDomainNames
--
--         , requestGetSdkType $
--             newGetSdkType
--
--         , requestPutGatewayResponse $
--             newPutGatewayResponse
--
--         , requestGetExport $
--             newGetExport
--
--         , requestGetClientCertificate $
--             newGetClientCertificate
--
--         , requestTestInvokeAuthorizer $
--             newTestInvokeAuthorizer
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
--         , requestPutIntegration $
--             newPutIntegration
--
--         , requestGetResources $
--             newGetResources
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
--         , requestPutIntegrationResponse $
--             newPutIntegrationResponse
--
--         , requestGetUsagePlanKeys $
--             newGetUsagePlanKeys
--
--         , requestCreateDocumentationPart $
--             newCreateDocumentationPart
--
--         , requestGetStages $
--             newGetStages
--
--         , requestGetDomainName $
--             newGetDomainName
--
--         , requestGetModelTemplate $
--             newGetModelTemplate
--
--         , requestDeleteRestApi $
--             newDeleteRestApi
--
--         , requestGetMethod $
--             newGetMethod
--
--         , requestUpdateRestApi $
--             newUpdateRestApi
--
--         , requestCreateVpcLink $
--             newCreateVpcLink
--
--         , requestGetRequestValidator $
--             newGetRequestValidator
--
--         , requestUpdateDocumentationVersion $
--             newUpdateDocumentationVersion
--
--         , requestImportApiKeys $
--             newImportApiKeys
--
--         , requestDeleteDocumentationVersion $
--             newDeleteDocumentationVersion
--
--         , requestPutMethod $
--             newPutMethod
--
--         , requestDeleteApiKey $
--             newDeleteApiKey
--
--         , requestFlushStageAuthorizersCache $
--             newFlushStageAuthorizersCache
--
--         , requestUpdateApiKey $
--             newUpdateApiKey
--
--         , requestGetRestApi $
--             newGetRestApi
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestCreateApiKey $
--             newCreateApiKey
--
--         , requestDeleteUsagePlanKey $
--             newDeleteUsagePlanKey
--
--         , requestUpdateAccount $
--             newUpdateAccount
--
--         , requestGetVpcLinks $
--             newGetVpcLinks
--
--         , requestUpdateAuthorizer $
--             newUpdateAuthorizer
--
--         , requestCreateBasePathMapping $
--             newCreateBasePathMapping
--
--         , requestGetDocumentationParts $
--             newGetDocumentationParts
--
--         , requestDeleteAuthorizer $
--             newDeleteAuthorizer
--
--         , requestDeleteClientCertificate $
--             newDeleteClientCertificate
--
--         , requestDeleteMethodResponse $
--             newDeleteMethodResponse
--
--         , requestDeleteBasePathMapping $
--             newDeleteBasePathMapping
--
--         , requestGetDocumentationVersions $
--             newGetDocumentationVersions
--
--         , requestUpdateBasePathMapping $
--             newUpdateBasePathMapping
--
--         , requestUpdateClientCertificate $
--             newUpdateClientCertificate
--
--         , requestUpdateMethodResponse $
--             newUpdateMethodResponse
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestGetApiKeys $
--             newGetApiKeys
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
--         , requestGetModels $
--             newGetModels
--
--         , requestGetStage $
--             newGetStage
--
--         , requestDeleteGatewayResponse $
--             newDeleteGatewayResponse
--
--           ]

--     , testGroup "response"
--         [ responseGenerateClientCertificate $
--             newClientCertificate
--
--         , responseUpdateIntegration $
--             newIntegration
--
--         , responseDeleteUsagePlan $
--             newDeleteUsagePlanResponse
--
--         , responseDeleteIntegration $
--             newDeleteIntegrationResponse'
--
--         , responseGetUsagePlanKey $
--             newUsagePlanKey
--
--         , responseGetAuthorizer $
--             newAuthorizer
--
--         , responseUpdateUsagePlan $
--             newUsagePlan
--
--         , responseGetDeployments $
--             newGetDeploymentsResponse
--
--         , responseDeleteIntegrationResponse $
--             newDeleteIntegrationResponseResponse
--
--         , responseFlushStageCache $
--             newFlushStageCacheResponse
--
--         , responseDeleteVpcLink $
--             newDeleteVpcLinkResponse
--
--         , responseDeleteDocumentationPart $
--             newDeleteDocumentationPartResponse
--
--         , responseUpdateVpcLink $
--             newVpcLink
--
--         , responseCreateRestApi $
--             newRestApi
--
--         , responseUpdateUsage $
--             newUsage
--
--         , responseUpdateIntegrationResponse $
--             newIntegrationResponse
--
--         , responseUpdateDocumentationPart $
--             newDocumentationPart
--
--         , responseGetSdkTypes $
--             newGetSdkTypesResponse
--
--         , responseGetBasePathMappings $
--             newGetBasePathMappingsResponse
--
--         , responseGetModel $
--             newModel
--
--         , responseGetClientCertificates $
--             newGetClientCertificatesResponse
--
--         , responseTestInvokeMethod $
--             newTestInvokeMethodResponse
--
--         , responsePutRestApi $
--             newRestApi
--
--         , responseGetApiKey $
--             newApiKey
--
--         , responseGetGatewayResponses $
--             newGetGatewayResponsesResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseDeleteMethod $
--             newDeleteMethodResponse'
--
--         , responseGetDocumentationVersion $
--             newDocumentationVersion
--
--         , responseUpdateMethod $
--             newMethod
--
--         , responseUpdateModel $
--             newModel
--
--         , responseUpdateDomainName $
--             newDomainName
--
--         , responseCreateResource $
--             newResource
--
--         , responseDeleteDomainName $
--             newDeleteDomainNameResponse
--
--         , responseUpdateRequestValidator $
--             newRequestValidator
--
--         , responseDeleteRequestValidator $
--             newDeleteRequestValidatorResponse
--
--         , responseGetUsagePlans $
--             newGetUsagePlansResponse
--
--         , responseCreateRequestValidator $
--             newRequestValidator
--
--         , responseCreateDomainName $
--             newDomainName
--
--         , responseGetVpcLink $
--             newVpcLink
--
--         , responseUpdateResource $
--             newResource
--
--         , responseGetDocumentationPart $
--             newDocumentationPart
--
--         , responseGetUsage $
--             newUsage
--
--         , responseImportDocumentationParts $
--             newImportDocumentationPartsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseGetIntegrationResponse $
--             newIntegrationResponse
--
--         , responseDeleteDeployment $
--             newDeleteDeploymentResponse
--
--         , responseCreateStage $
--             newStage
--
--         , responseGetIntegration $
--             newIntegration
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUpdateDeployment $
--             newDeployment
--
--         , responseGetUsagePlan $
--             newUsagePlan
--
--         , responseGetRestApis $
--             newGetRestApisResponse
--
--         , responseCreateAuthorizer $
--             newAuthorizer
--
--         , responseDeleteStage $
--             newDeleteStageResponse
--
--         , responseUpdateStage $
--             newStage
--
--         , responseCreateUsagePlanKey $
--             newUsagePlanKey
--
--         , responseGetGatewayResponse $
--             newGatewayResponse
--
--         , responseImportRestApi $
--             newRestApi
--
--         , responsePutMethodResponse $
--             newMethodResponse
--
--         , responseGetBasePathMapping $
--             newBasePathMapping
--
--         , responseGetRequestValidators $
--             newGetRequestValidatorsResponse
--
--         , responseGetDomainNames $
--             newGetDomainNamesResponse
--
--         , responseGetSdkType $
--             newSdkType
--
--         , responsePutGatewayResponse $
--             newGatewayResponse
--
--         , responseGetExport $
--             newGetExportResponse
--
--         , responseGetClientCertificate $
--             newClientCertificate
--
--         , responseTestInvokeAuthorizer $
--             newTestInvokeAuthorizerResponse
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
--         , responsePutIntegration $
--             newIntegration
--
--         , responseGetResources $
--             newGetResourcesResponse
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
--         , responsePutIntegrationResponse $
--             newIntegrationResponse
--
--         , responseGetUsagePlanKeys $
--             newGetUsagePlanKeysResponse
--
--         , responseCreateDocumentationPart $
--             newDocumentationPart
--
--         , responseGetStages $
--             newGetStagesResponse
--
--         , responseGetDomainName $
--             newDomainName
--
--         , responseGetModelTemplate $
--             newGetModelTemplateResponse
--
--         , responseDeleteRestApi $
--             newDeleteRestApiResponse
--
--         , responseGetMethod $
--             newMethod
--
--         , responseUpdateRestApi $
--             newRestApi
--
--         , responseCreateVpcLink $
--             newVpcLink
--
--         , responseGetRequestValidator $
--             newRequestValidator
--
--         , responseUpdateDocumentationVersion $
--             newDocumentationVersion
--
--         , responseImportApiKeys $
--             newImportApiKeysResponse
--
--         , responseDeleteDocumentationVersion $
--             newDeleteDocumentationVersionResponse
--
--         , responsePutMethod $
--             newMethod
--
--         , responseDeleteApiKey $
--             newDeleteApiKeyResponse
--
--         , responseFlushStageAuthorizersCache $
--             newFlushStageAuthorizersCacheResponse
--
--         , responseUpdateApiKey $
--             newApiKey
--
--         , responseGetRestApi $
--             newRestApi
--
--         , responseCreateModel $
--             newModel
--
--         , responseCreateApiKey $
--             newApiKey
--
--         , responseDeleteUsagePlanKey $
--             newDeleteUsagePlanKeyResponse
--
--         , responseUpdateAccount $
--             newAccount
--
--         , responseGetVpcLinks $
--             newGetVpcLinksResponse
--
--         , responseUpdateAuthorizer $
--             newAuthorizer
--
--         , responseCreateBasePathMapping $
--             newBasePathMapping
--
--         , responseGetDocumentationParts $
--             newGetDocumentationPartsResponse
--
--         , responseDeleteAuthorizer $
--             newDeleteAuthorizerResponse
--
--         , responseDeleteClientCertificate $
--             newDeleteClientCertificateResponse
--
--         , responseDeleteMethodResponse $
--             newDeleteMethodResponseResponse
--
--         , responseDeleteBasePathMapping $
--             newDeleteBasePathMappingResponse
--
--         , responseGetDocumentationVersions $
--             newGetDocumentationVersionsResponse
--
--         , responseUpdateBasePathMapping $
--             newBasePathMapping
--
--         , responseUpdateClientCertificate $
--             newClientCertificate
--
--         , responseUpdateMethodResponse $
--             newMethodResponse
--
--         , responseCreateDeployment $
--             newDeployment
--
--         , responseGetApiKeys $
--             newGetApiKeysResponse
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
--         , responseGetModels $
--             newGetModelsResponse
--
--         , responseGetStage $
--             newStage
--
--         , responseDeleteGatewayResponse $
--             newDeleteGatewayResponseResponse
--
--           ]
--     ]

-- Requests

requestGenerateClientCertificate :: GenerateClientCertificate -> TestTree
requestGenerateClientCertificate =
  req
    "GenerateClientCertificate"
    "fixture/GenerateClientCertificate.yaml"

requestUpdateIntegration :: UpdateIntegration -> TestTree
requestUpdateIntegration =
  req
    "UpdateIntegration"
    "fixture/UpdateIntegration.yaml"

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

requestGetUsagePlanKey :: GetUsagePlanKey -> TestTree
requestGetUsagePlanKey =
  req
    "GetUsagePlanKey"
    "fixture/GetUsagePlanKey.yaml"

requestGetAuthorizer :: GetAuthorizer -> TestTree
requestGetAuthorizer =
  req
    "GetAuthorizer"
    "fixture/GetAuthorizer.yaml"

requestUpdateUsagePlan :: UpdateUsagePlan -> TestTree
requestUpdateUsagePlan =
  req
    "UpdateUsagePlan"
    "fixture/UpdateUsagePlan.yaml"

requestGetDeployments :: GetDeployments -> TestTree
requestGetDeployments =
  req
    "GetDeployments"
    "fixture/GetDeployments.yaml"

requestDeleteIntegrationResponse :: DeleteIntegrationResponse -> TestTree
requestDeleteIntegrationResponse =
  req
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.yaml"

requestFlushStageCache :: FlushStageCache -> TestTree
requestFlushStageCache =
  req
    "FlushStageCache"
    "fixture/FlushStageCache.yaml"

requestDeleteVpcLink :: DeleteVpcLink -> TestTree
requestDeleteVpcLink =
  req
    "DeleteVpcLink"
    "fixture/DeleteVpcLink.yaml"

requestDeleteDocumentationPart :: DeleteDocumentationPart -> TestTree
requestDeleteDocumentationPart =
  req
    "DeleteDocumentationPart"
    "fixture/DeleteDocumentationPart.yaml"

requestUpdateVpcLink :: UpdateVpcLink -> TestTree
requestUpdateVpcLink =
  req
    "UpdateVpcLink"
    "fixture/UpdateVpcLink.yaml"

requestCreateRestApi :: CreateRestApi -> TestTree
requestCreateRestApi =
  req
    "CreateRestApi"
    "fixture/CreateRestApi.yaml"

requestUpdateUsage :: UpdateUsage -> TestTree
requestUpdateUsage =
  req
    "UpdateUsage"
    "fixture/UpdateUsage.yaml"

requestUpdateIntegrationResponse :: UpdateIntegrationResponse -> TestTree
requestUpdateIntegrationResponse =
  req
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.yaml"

requestUpdateDocumentationPart :: UpdateDocumentationPart -> TestTree
requestUpdateDocumentationPart =
  req
    "UpdateDocumentationPart"
    "fixture/UpdateDocumentationPart.yaml"

requestGetSdkTypes :: GetSdkTypes -> TestTree
requestGetSdkTypes =
  req
    "GetSdkTypes"
    "fixture/GetSdkTypes.yaml"

requestGetBasePathMappings :: GetBasePathMappings -> TestTree
requestGetBasePathMappings =
  req
    "GetBasePathMappings"
    "fixture/GetBasePathMappings.yaml"

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

requestTestInvokeMethod :: TestInvokeMethod -> TestTree
requestTestInvokeMethod =
  req
    "TestInvokeMethod"
    "fixture/TestInvokeMethod.yaml"

requestPutRestApi :: PutRestApi -> TestTree
requestPutRestApi =
  req
    "PutRestApi"
    "fixture/PutRestApi.yaml"

requestGetApiKey :: GetApiKey -> TestTree
requestGetApiKey =
  req
    "GetApiKey"
    "fixture/GetApiKey.yaml"

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

requestDeleteMethod :: DeleteMethod -> TestTree
requestDeleteMethod =
  req
    "DeleteMethod"
    "fixture/DeleteMethod.yaml"

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

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel =
  req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestUpdateDomainName :: UpdateDomainName -> TestTree
requestUpdateDomainName =
  req
    "UpdateDomainName"
    "fixture/UpdateDomainName.yaml"

requestCreateResource :: CreateResource -> TestTree
requestCreateResource =
  req
    "CreateResource"
    "fixture/CreateResource.yaml"

requestDeleteDomainName :: DeleteDomainName -> TestTree
requestDeleteDomainName =
  req
    "DeleteDomainName"
    "fixture/DeleteDomainName.yaml"

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

requestGetUsagePlans :: GetUsagePlans -> TestTree
requestGetUsagePlans =
  req
    "GetUsagePlans"
    "fixture/GetUsagePlans.yaml"

requestCreateRequestValidator :: CreateRequestValidator -> TestTree
requestCreateRequestValidator =
  req
    "CreateRequestValidator"
    "fixture/CreateRequestValidator.yaml"

requestCreateDomainName :: CreateDomainName -> TestTree
requestCreateDomainName =
  req
    "CreateDomainName"
    "fixture/CreateDomainName.yaml"

requestGetVpcLink :: GetVpcLink -> TestTree
requestGetVpcLink =
  req
    "GetVpcLink"
    "fixture/GetVpcLink.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestGetDocumentationPart :: GetDocumentationPart -> TestTree
requestGetDocumentationPart =
  req
    "GetDocumentationPart"
    "fixture/GetDocumentationPart.yaml"

requestGetUsage :: GetUsage -> TestTree
requestGetUsage =
  req
    "GetUsage"
    "fixture/GetUsage.yaml"

requestImportDocumentationParts :: ImportDocumentationParts -> TestTree
requestImportDocumentationParts =
  req
    "ImportDocumentationParts"
    "fixture/ImportDocumentationParts.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestGetIntegrationResponse :: GetIntegrationResponse -> TestTree
requestGetIntegrationResponse =
  req
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.yaml"

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

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration =
  req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestGetRestApis :: GetRestApis -> TestTree
requestGetRestApis =
  req
    "GetRestApis"
    "fixture/GetRestApis.yaml"

requestCreateAuthorizer :: CreateAuthorizer -> TestTree
requestCreateAuthorizer =
  req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

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

requestCreateUsagePlanKey :: CreateUsagePlanKey -> TestTree
requestCreateUsagePlanKey =
  req
    "CreateUsagePlanKey"
    "fixture/CreateUsagePlanKey.yaml"

requestGetGatewayResponse :: GetGatewayResponse -> TestTree
requestGetGatewayResponse =
  req
    "GetGatewayResponse"
    "fixture/GetGatewayResponse.yaml"

requestImportRestApi :: ImportRestApi -> TestTree
requestImportRestApi =
  req
    "ImportRestApi"
    "fixture/ImportRestApi.yaml"

requestPutMethodResponse :: PutMethodResponse -> TestTree
requestPutMethodResponse =
  req
    "PutMethodResponse"
    "fixture/PutMethodResponse.yaml"

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

requestGetDomainNames :: GetDomainNames -> TestTree
requestGetDomainNames =
  req
    "GetDomainNames"
    "fixture/GetDomainNames.yaml"

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

requestGetExport :: GetExport -> TestTree
requestGetExport =
  req
    "GetExport"
    "fixture/GetExport.yaml"

requestGetClientCertificate :: GetClientCertificate -> TestTree
requestGetClientCertificate =
  req
    "GetClientCertificate"
    "fixture/GetClientCertificate.yaml"

requestTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
requestTestInvokeAuthorizer =
  req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

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

requestCreateDocumentationPart :: CreateDocumentationPart -> TestTree
requestCreateDocumentationPart =
  req
    "CreateDocumentationPart"
    "fixture/CreateDocumentationPart.yaml"

requestGetStages :: GetStages -> TestTree
requestGetStages =
  req
    "GetStages"
    "fixture/GetStages.yaml"

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

requestDeleteRestApi :: DeleteRestApi -> TestTree
requestDeleteRestApi =
  req
    "DeleteRestApi"
    "fixture/DeleteRestApi.yaml"

requestGetMethod :: GetMethod -> TestTree
requestGetMethod =
  req
    "GetMethod"
    "fixture/GetMethod.yaml"

requestUpdateRestApi :: UpdateRestApi -> TestTree
requestUpdateRestApi =
  req
    "UpdateRestApi"
    "fixture/UpdateRestApi.yaml"

requestCreateVpcLink :: CreateVpcLink -> TestTree
requestCreateVpcLink =
  req
    "CreateVpcLink"
    "fixture/CreateVpcLink.yaml"

requestGetRequestValidator :: GetRequestValidator -> TestTree
requestGetRequestValidator =
  req
    "GetRequestValidator"
    "fixture/GetRequestValidator.yaml"

requestUpdateDocumentationVersion :: UpdateDocumentationVersion -> TestTree
requestUpdateDocumentationVersion =
  req
    "UpdateDocumentationVersion"
    "fixture/UpdateDocumentationVersion.yaml"

requestImportApiKeys :: ImportApiKeys -> TestTree
requestImportApiKeys =
  req
    "ImportApiKeys"
    "fixture/ImportApiKeys.yaml"

requestDeleteDocumentationVersion :: DeleteDocumentationVersion -> TestTree
requestDeleteDocumentationVersion =
  req
    "DeleteDocumentationVersion"
    "fixture/DeleteDocumentationVersion.yaml"

requestPutMethod :: PutMethod -> TestTree
requestPutMethod =
  req
    "PutMethod"
    "fixture/PutMethod.yaml"

requestDeleteApiKey :: DeleteApiKey -> TestTree
requestDeleteApiKey =
  req
    "DeleteApiKey"
    "fixture/DeleteApiKey.yaml"

requestFlushStageAuthorizersCache :: FlushStageAuthorizersCache -> TestTree
requestFlushStageAuthorizersCache =
  req
    "FlushStageAuthorizersCache"
    "fixture/FlushStageAuthorizersCache.yaml"

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

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestCreateApiKey :: CreateApiKey -> TestTree
requestCreateApiKey =
  req
    "CreateApiKey"
    "fixture/CreateApiKey.yaml"

requestDeleteUsagePlanKey :: DeleteUsagePlanKey -> TestTree
requestDeleteUsagePlanKey =
  req
    "DeleteUsagePlanKey"
    "fixture/DeleteUsagePlanKey.yaml"

requestUpdateAccount :: UpdateAccount -> TestTree
requestUpdateAccount =
  req
    "UpdateAccount"
    "fixture/UpdateAccount.yaml"

requestGetVpcLinks :: GetVpcLinks -> TestTree
requestGetVpcLinks =
  req
    "GetVpcLinks"
    "fixture/GetVpcLinks.yaml"

requestUpdateAuthorizer :: UpdateAuthorizer -> TestTree
requestUpdateAuthorizer =
  req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

requestCreateBasePathMapping :: CreateBasePathMapping -> TestTree
requestCreateBasePathMapping =
  req
    "CreateBasePathMapping"
    "fixture/CreateBasePathMapping.yaml"

requestGetDocumentationParts :: GetDocumentationParts -> TestTree
requestGetDocumentationParts =
  req
    "GetDocumentationParts"
    "fixture/GetDocumentationParts.yaml"

requestDeleteAuthorizer :: DeleteAuthorizer -> TestTree
requestDeleteAuthorizer =
  req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

requestDeleteClientCertificate :: DeleteClientCertificate -> TestTree
requestDeleteClientCertificate =
  req
    "DeleteClientCertificate"
    "fixture/DeleteClientCertificate.yaml"

requestDeleteMethodResponse :: DeleteMethodResponse -> TestTree
requestDeleteMethodResponse =
  req
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.yaml"

requestDeleteBasePathMapping :: DeleteBasePathMapping -> TestTree
requestDeleteBasePathMapping =
  req
    "DeleteBasePathMapping"
    "fixture/DeleteBasePathMapping.yaml"

requestGetDocumentationVersions :: GetDocumentationVersions -> TestTree
requestGetDocumentationVersions =
  req
    "GetDocumentationVersions"
    "fixture/GetDocumentationVersions.yaml"

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

requestUpdateMethodResponse :: UpdateMethodResponse -> TestTree
requestUpdateMethodResponse =
  req
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestGetApiKeys :: GetApiKeys -> TestTree
requestGetApiKeys =
  req
    "GetApiKeys"
    "fixture/GetApiKeys.yaml"

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

requestGetModels :: GetModels -> TestTree
requestGetModels =
  req
    "GetModels"
    "fixture/GetModels.yaml"

requestGetStage :: GetStage -> TestTree
requestGetStage =
  req
    "GetStage"
    "fixture/GetStage.yaml"

requestDeleteGatewayResponse :: DeleteGatewayResponse -> TestTree
requestDeleteGatewayResponse =
  req
    "DeleteGatewayResponse"
    "fixture/DeleteGatewayResponse.yaml"

-- Responses

responseGenerateClientCertificate :: ClientCertificate -> TestTree
responseGenerateClientCertificate =
  res
    "GenerateClientCertificateResponse"
    "fixture/GenerateClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GenerateClientCertificate)

responseUpdateIntegration :: Integration -> TestTree
responseUpdateIntegration =
  res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIntegration)

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

responseGetUsagePlanKey :: UsagePlanKey -> TestTree
responseGetUsagePlanKey =
  res
    "GetUsagePlanKeyResponse"
    "fixture/GetUsagePlanKeyResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsagePlanKey)

responseGetAuthorizer :: Authorizer -> TestTree
responseGetAuthorizer =
  res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy GetAuthorizer)

responseUpdateUsagePlan :: UsagePlan -> TestTree
responseUpdateUsagePlan =
  res
    "UpdateUsagePlanResponse"
    "fixture/UpdateUsagePlanResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUsagePlan)

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments =
  res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeployments)

responseDeleteIntegrationResponse :: DeleteIntegrationResponseResponse -> TestTree
responseDeleteIntegrationResponse =
  res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIntegrationResponse)

responseFlushStageCache :: FlushStageCacheResponse -> TestTree
responseFlushStageCache =
  res
    "FlushStageCacheResponse"
    "fixture/FlushStageCacheResponse.proto"
    defaultService
    (Proxy :: Proxy FlushStageCache)

responseDeleteVpcLink :: DeleteVpcLinkResponse -> TestTree
responseDeleteVpcLink =
  res
    "DeleteVpcLinkResponse"
    "fixture/DeleteVpcLinkResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVpcLink)

responseDeleteDocumentationPart :: DeleteDocumentationPartResponse -> TestTree
responseDeleteDocumentationPart =
  res
    "DeleteDocumentationPartResponse"
    "fixture/DeleteDocumentationPartResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDocumentationPart)

responseUpdateVpcLink :: VpcLink -> TestTree
responseUpdateVpcLink =
  res
    "UpdateVpcLinkResponse"
    "fixture/UpdateVpcLinkResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateVpcLink)

responseCreateRestApi :: RestApi -> TestTree
responseCreateRestApi =
  res
    "CreateRestApiResponse"
    "fixture/CreateRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRestApi)

responseUpdateUsage :: Usage -> TestTree
responseUpdateUsage =
  res
    "UpdateUsageResponse"
    "fixture/UpdateUsageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUsage)

responseUpdateIntegrationResponse :: IntegrationResponse -> TestTree
responseUpdateIntegrationResponse =
  res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIntegrationResponse)

responseUpdateDocumentationPart :: DocumentationPart -> TestTree
responseUpdateDocumentationPart =
  res
    "UpdateDocumentationPartResponse"
    "fixture/UpdateDocumentationPartResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentationPart)

responseGetSdkTypes :: GetSdkTypesResponse -> TestTree
responseGetSdkTypes =
  res
    "GetSdkTypesResponse"
    "fixture/GetSdkTypesResponse.proto"
    defaultService
    (Proxy :: Proxy GetSdkTypes)

responseGetBasePathMappings :: GetBasePathMappingsResponse -> TestTree
responseGetBasePathMappings =
  res
    "GetBasePathMappingsResponse"
    "fixture/GetBasePathMappingsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBasePathMappings)

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

responseTestInvokeMethod :: TestInvokeMethodResponse -> TestTree
responseTestInvokeMethod =
  res
    "TestInvokeMethodResponse"
    "fixture/TestInvokeMethodResponse.proto"
    defaultService
    (Proxy :: Proxy TestInvokeMethod)

responsePutRestApi :: RestApi -> TestTree
responsePutRestApi =
  res
    "PutRestApiResponse"
    "fixture/PutRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy PutRestApi)

responseGetApiKey :: ApiKey -> TestTree
responseGetApiKey =
  res
    "GetApiKeyResponse"
    "fixture/GetApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy GetApiKey)

responseGetGatewayResponses :: GetGatewayResponsesResponse -> TestTree
responseGetGatewayResponses =
  res
    "GetGatewayResponsesResponse"
    "fixture/GetGatewayResponsesResponse.proto"
    defaultService
    (Proxy :: Proxy GetGatewayResponses)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModel)

responseDeleteMethod :: DeleteMethodResponse' -> TestTree
responseDeleteMethod =
  res
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMethod)

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

responseUpdateModel :: Model -> TestTree
responseUpdateModel =
  res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateModel)

responseUpdateDomainName :: DomainName -> TestTree
responseUpdateDomainName =
  res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainName)

responseCreateResource :: Resource -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResource)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName =
  res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomainName)

responseUpdateRequestValidator :: RequestValidator -> TestTree
responseUpdateRequestValidator =
  res
    "UpdateRequestValidatorResponse"
    "fixture/UpdateRequestValidatorResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRequestValidator)

responseDeleteRequestValidator :: DeleteRequestValidatorResponse -> TestTree
responseDeleteRequestValidator =
  res
    "DeleteRequestValidatorResponse"
    "fixture/DeleteRequestValidatorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRequestValidator)

responseGetUsagePlans :: GetUsagePlansResponse -> TestTree
responseGetUsagePlans =
  res
    "GetUsagePlansResponse"
    "fixture/GetUsagePlansResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsagePlans)

responseCreateRequestValidator :: RequestValidator -> TestTree
responseCreateRequestValidator =
  res
    "CreateRequestValidatorResponse"
    "fixture/CreateRequestValidatorResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRequestValidator)

responseCreateDomainName :: DomainName -> TestTree
responseCreateDomainName =
  res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomainName)

responseGetVpcLink :: VpcLink -> TestTree
responseGetVpcLink =
  res
    "GetVpcLinkResponse"
    "fixture/GetVpcLinkResponse.proto"
    defaultService
    (Proxy :: Proxy GetVpcLink)

responseUpdateResource :: Resource -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResource)

responseGetDocumentationPart :: DocumentationPart -> TestTree
responseGetDocumentationPart =
  res
    "GetDocumentationPartResponse"
    "fixture/GetDocumentationPartResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentationPart)

responseGetUsage :: Usage -> TestTree
responseGetUsage =
  res
    "GetUsageResponse"
    "fixture/GetUsageResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsage)

responseImportDocumentationParts :: ImportDocumentationPartsResponse -> TestTree
responseImportDocumentationParts =
  res
    "ImportDocumentationPartsResponse"
    "fixture/ImportDocumentationPartsResponse.proto"
    defaultService
    (Proxy :: Proxy ImportDocumentationParts)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResource)

responseGetIntegrationResponse :: IntegrationResponse -> TestTree
responseGetIntegrationResponse =
  res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    defaultService
    (Proxy :: Proxy GetIntegrationResponse)

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

responseGetIntegration :: Integration -> TestTree
responseGetIntegration =
  res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    defaultService
    (Proxy :: Proxy GetIntegration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

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

responseGetRestApis :: GetRestApisResponse -> TestTree
responseGetRestApis =
  res
    "GetRestApisResponse"
    "fixture/GetRestApisResponse.proto"
    defaultService
    (Proxy :: Proxy GetRestApis)

responseCreateAuthorizer :: Authorizer -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAuthorizer)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage =
  res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStage)

responseUpdateStage :: Stage -> TestTree
responseUpdateStage =
  res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStage)

responseCreateUsagePlanKey :: UsagePlanKey -> TestTree
responseCreateUsagePlanKey =
  res
    "CreateUsagePlanKeyResponse"
    "fixture/CreateUsagePlanKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUsagePlanKey)

responseGetGatewayResponse :: GatewayResponse -> TestTree
responseGetGatewayResponse =
  res
    "GetGatewayResponseResponse"
    "fixture/GetGatewayResponseResponse.proto"
    defaultService
    (Proxy :: Proxy GetGatewayResponse)

responseImportRestApi :: RestApi -> TestTree
responseImportRestApi =
  res
    "ImportRestApiResponse"
    "fixture/ImportRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy ImportRestApi)

responsePutMethodResponse :: MethodResponse -> TestTree
responsePutMethodResponse =
  res
    "PutMethodResponseResponse"
    "fixture/PutMethodResponseResponse.proto"
    defaultService
    (Proxy :: Proxy PutMethodResponse)

responseGetBasePathMapping :: BasePathMapping -> TestTree
responseGetBasePathMapping =
  res
    "GetBasePathMappingResponse"
    "fixture/GetBasePathMappingResponse.proto"
    defaultService
    (Proxy :: Proxy GetBasePathMapping)

responseGetRequestValidators :: GetRequestValidatorsResponse -> TestTree
responseGetRequestValidators =
  res
    "GetRequestValidatorsResponse"
    "fixture/GetRequestValidatorsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRequestValidators)

responseGetDomainNames :: GetDomainNamesResponse -> TestTree
responseGetDomainNames =
  res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomainNames)

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

responseGetExport :: GetExportResponse -> TestTree
responseGetExport =
  res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    defaultService
    (Proxy :: Proxy GetExport)

responseGetClientCertificate :: ClientCertificate -> TestTree
responseGetClientCertificate =
  res
    "GetClientCertificateResponse"
    "fixture/GetClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GetClientCertificate)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy TestInvokeAuthorizer)

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

responsePutIntegrationResponse :: IntegrationResponse -> TestTree
responsePutIntegrationResponse =
  res
    "PutIntegrationResponseResponse"
    "fixture/PutIntegrationResponseResponse.proto"
    defaultService
    (Proxy :: Proxy PutIntegrationResponse)

responseGetUsagePlanKeys :: GetUsagePlanKeysResponse -> TestTree
responseGetUsagePlanKeys =
  res
    "GetUsagePlanKeysResponse"
    "fixture/GetUsagePlanKeysResponse.proto"
    defaultService
    (Proxy :: Proxy GetUsagePlanKeys)

responseCreateDocumentationPart :: DocumentationPart -> TestTree
responseCreateDocumentationPart =
  res
    "CreateDocumentationPartResponse"
    "fixture/CreateDocumentationPartResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDocumentationPart)

responseGetStages :: GetStagesResponse -> TestTree
responseGetStages =
  res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    defaultService
    (Proxy :: Proxy GetStages)

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

responseDeleteRestApi :: DeleteRestApiResponse -> TestTree
responseDeleteRestApi =
  res
    "DeleteRestApiResponse"
    "fixture/DeleteRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRestApi)

responseGetMethod :: Method -> TestTree
responseGetMethod =
  res
    "GetMethodResponse"
    "fixture/GetMethodResponse.proto"
    defaultService
    (Proxy :: Proxy GetMethod)

responseUpdateRestApi :: RestApi -> TestTree
responseUpdateRestApi =
  res
    "UpdateRestApiResponse"
    "fixture/UpdateRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRestApi)

responseCreateVpcLink :: VpcLink -> TestTree
responseCreateVpcLink =
  res
    "CreateVpcLinkResponse"
    "fixture/CreateVpcLinkResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVpcLink)

responseGetRequestValidator :: RequestValidator -> TestTree
responseGetRequestValidator =
  res
    "GetRequestValidatorResponse"
    "fixture/GetRequestValidatorResponse.proto"
    defaultService
    (Proxy :: Proxy GetRequestValidator)

responseUpdateDocumentationVersion :: DocumentationVersion -> TestTree
responseUpdateDocumentationVersion =
  res
    "UpdateDocumentationVersionResponse"
    "fixture/UpdateDocumentationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDocumentationVersion)

responseImportApiKeys :: ImportApiKeysResponse -> TestTree
responseImportApiKeys =
  res
    "ImportApiKeysResponse"
    "fixture/ImportApiKeysResponse.proto"
    defaultService
    (Proxy :: Proxy ImportApiKeys)

responseDeleteDocumentationVersion :: DeleteDocumentationVersionResponse -> TestTree
responseDeleteDocumentationVersion =
  res
    "DeleteDocumentationVersionResponse"
    "fixture/DeleteDocumentationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDocumentationVersion)

responsePutMethod :: Method -> TestTree
responsePutMethod =
  res
    "PutMethodResponse"
    "fixture/PutMethodResponse.proto"
    defaultService
    (Proxy :: Proxy PutMethod)

responseDeleteApiKey :: DeleteApiKeyResponse -> TestTree
responseDeleteApiKey =
  res
    "DeleteApiKeyResponse"
    "fixture/DeleteApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApiKey)

responseFlushStageAuthorizersCache :: FlushStageAuthorizersCacheResponse -> TestTree
responseFlushStageAuthorizersCache =
  res
    "FlushStageAuthorizersCacheResponse"
    "fixture/FlushStageAuthorizersCacheResponse.proto"
    defaultService
    (Proxy :: Proxy FlushStageAuthorizersCache)

responseUpdateApiKey :: ApiKey -> TestTree
responseUpdateApiKey =
  res
    "UpdateApiKeyResponse"
    "fixture/UpdateApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApiKey)

responseGetRestApi :: RestApi -> TestTree
responseGetRestApi =
  res
    "GetRestApiResponse"
    "fixture/GetRestApiResponse.proto"
    defaultService
    (Proxy :: Proxy GetRestApi)

responseCreateModel :: Model -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModel)

responseCreateApiKey :: ApiKey -> TestTree
responseCreateApiKey =
  res
    "CreateApiKeyResponse"
    "fixture/CreateApiKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApiKey)

responseDeleteUsagePlanKey :: DeleteUsagePlanKeyResponse -> TestTree
responseDeleteUsagePlanKey =
  res
    "DeleteUsagePlanKeyResponse"
    "fixture/DeleteUsagePlanKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUsagePlanKey)

responseUpdateAccount :: Account -> TestTree
responseUpdateAccount =
  res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAccount)

responseGetVpcLinks :: GetVpcLinksResponse -> TestTree
responseGetVpcLinks =
  res
    "GetVpcLinksResponse"
    "fixture/GetVpcLinksResponse.proto"
    defaultService
    (Proxy :: Proxy GetVpcLinks)

responseUpdateAuthorizer :: Authorizer -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAuthorizer)

responseCreateBasePathMapping :: BasePathMapping -> TestTree
responseCreateBasePathMapping =
  res
    "CreateBasePathMappingResponse"
    "fixture/CreateBasePathMappingResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBasePathMapping)

responseGetDocumentationParts :: GetDocumentationPartsResponse -> TestTree
responseGetDocumentationParts =
  res
    "GetDocumentationPartsResponse"
    "fixture/GetDocumentationPartsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentationParts)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAuthorizer)

responseDeleteClientCertificate :: DeleteClientCertificateResponse -> TestTree
responseDeleteClientCertificate =
  res
    "DeleteClientCertificateResponse"
    "fixture/DeleteClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteClientCertificate)

responseDeleteMethodResponse :: DeleteMethodResponseResponse -> TestTree
responseDeleteMethodResponse =
  res
    "DeleteMethodResponseResponse"
    "fixture/DeleteMethodResponseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMethodResponse)

responseDeleteBasePathMapping :: DeleteBasePathMappingResponse -> TestTree
responseDeleteBasePathMapping =
  res
    "DeleteBasePathMappingResponse"
    "fixture/DeleteBasePathMappingResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBasePathMapping)

responseGetDocumentationVersions :: GetDocumentationVersionsResponse -> TestTree
responseGetDocumentationVersions =
  res
    "GetDocumentationVersionsResponse"
    "fixture/GetDocumentationVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentationVersions)

responseUpdateBasePathMapping :: BasePathMapping -> TestTree
responseUpdateBasePathMapping =
  res
    "UpdateBasePathMappingResponse"
    "fixture/UpdateBasePathMappingResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBasePathMapping)

responseUpdateClientCertificate :: ClientCertificate -> TestTree
responseUpdateClientCertificate =
  res
    "UpdateClientCertificateResponse"
    "fixture/UpdateClientCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateClientCertificate)

responseUpdateMethodResponse :: MethodResponse -> TestTree
responseUpdateMethodResponse =
  res
    "UpdateMethodResponseResponse"
    "fixture/UpdateMethodResponseResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMethodResponse)

responseCreateDeployment :: Deployment -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeployment)

responseGetApiKeys :: GetApiKeysResponse -> TestTree
responseGetApiKeys =
  res
    "GetApiKeysResponse"
    "fixture/GetApiKeysResponse.proto"
    defaultService
    (Proxy :: Proxy GetApiKeys)

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

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels =
  res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    defaultService
    (Proxy :: Proxy GetModels)

responseGetStage :: Stage -> TestTree
responseGetStage =
  res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    defaultService
    (Proxy :: Proxy GetStage)

responseDeleteGatewayResponse :: DeleteGatewayResponseResponse -> TestTree
responseDeleteGatewayResponse =
  res
    "DeleteGatewayResponseResponse"
    "fixture/DeleteGatewayResponseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGatewayResponse)
