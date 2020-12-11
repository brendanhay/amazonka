{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.APIGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         , requestGetSDKType $
--             mkGetSDKType
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
--         , requestImportRestAPI $
--             mkImportRestAPI
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
--         , requestGetRestAPIs $
--             mkGetRestAPIs
--
--         , requestGetDocumentationVersions $
--             mkGetDocumentationVersions
--
--         , requestCreateDeployment $
--             mkCreateDeployment
--
--         , requestGetVPCLinks $
--             mkGetVPCLinks
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
--         , requestGetVPCLink $
--             mkGetVPCLink
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
--         , requestDeleteAPIKey $
--             mkDeleteAPIKey
--
--         , requestUpdateAPIKey $
--             mkUpdateAPIKey
--
--         , requestGetRestAPI $
--             mkGetRestAPI
--
--         , requestGetStages $
--             mkGetStages
--
--         , requestPutRestAPI $
--             mkPutRestAPI
--
--         , requestGetMethod $
--             mkGetMethod
--
--         , requestGetModel $
--             mkGetModel
--
--         , requestUpdateRestAPI $
--             mkUpdateRestAPI
--
--         , requestDeleteRestAPI $
--             mkDeleteRestAPI
--
--         , requestImportAPIKeys $
--             mkImportAPIKeys
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
--         , requestCreateVPCLink $
--             mkCreateVPCLink
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
--         , requestDeleteVPCLink $
--             mkDeleteVPCLink
--
--         , requestUpdateVPCLink $
--             mkUpdateVPCLink
--
--         , requestFlushStageCache $
--             mkFlushStageCache
--
--         , requestCreateRestAPI $
--             mkCreateRestAPI
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
--         , requestGetSDK $
--             mkGetSDK
--
--         , requestGetAPIKeys $
--             mkGetAPIKeys
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
--         , requestCreateAPIKey $
--             mkCreateAPIKey
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
--         , requestGetSDKTypes $
--             mkGetSDKTypes
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
--         , requestGetAPIKey $
--             mkGetAPIKey
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
--         , responseGetSDKType $
--             mkSDKType
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
--         , responseImportRestAPI $
--             mkRestAPI
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
--         , responseGetRestAPIs $
--             mkGetRestAPIsResponse
--
--         , responseGetDocumentationVersions $
--             mkGetDocumentationVersionsResponse
--
--         , responseCreateDeployment $
--             mkDeployment
--
--         , responseGetVPCLinks $
--             mkGetVPCLinksResponse
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
--         , responseGetVPCLink $
--             mkVPCLink
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
--         , responseDeleteAPIKey $
--             mkDeleteAPIKeyResponse
--
--         , responseUpdateAPIKey $
--             mkAPIKey
--
--         , responseGetRestAPI $
--             mkRestAPI
--
--         , responseGetStages $
--             mkGetStagesResponse
--
--         , responsePutRestAPI $
--             mkRestAPI
--
--         , responseGetMethod $
--             mkMethod
--
--         , responseGetModel $
--             mkModel
--
--         , responseUpdateRestAPI $
--             mkRestAPI
--
--         , responseDeleteRestAPI $
--             mkDeleteRestAPIResponse
--
--         , responseImportAPIKeys $
--             mkImportAPIKeysResponse
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
--         , responseCreateVPCLink $
--             mkVPCLink
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
--         , responseDeleteVPCLink $
--             mkDeleteVPCLinkResponse
--
--         , responseUpdateVPCLink $
--             mkVPCLink
--
--         , responseFlushStageCache $
--             mkFlushStageCacheResponse
--
--         , responseCreateRestAPI $
--             mkRestAPI
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
--         , responseGetSDK $
--             mkGetSDKResponse
--
--         , responseGetAPIKeys $
--             mkGetAPIKeysResponse
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
--         , responseCreateAPIKey $
--             mkAPIKey
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
--         , responseGetSDKTypes $
--             mkGetSDKTypesResponse
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
--         , responseGetAPIKey $
--             mkAPIKey
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

requestGetSDKType :: GetSDKType -> TestTree
requestGetSDKType =
  req
    "GetSDKType"
    "fixture/GetSDKType.yaml"

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

requestImportRestAPI :: ImportRestAPI -> TestTree
requestImportRestAPI =
  req
    "ImportRestAPI"
    "fixture/ImportRestAPI.yaml"

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

requestGetRestAPIs :: GetRestAPIs -> TestTree
requestGetRestAPIs =
  req
    "GetRestAPIs"
    "fixture/GetRestAPIs.yaml"

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

requestGetVPCLinks :: GetVPCLinks -> TestTree
requestGetVPCLinks =
  req
    "GetVPCLinks"
    "fixture/GetVPCLinks.yaml"

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

requestGetVPCLink :: GetVPCLink -> TestTree
requestGetVPCLink =
  req
    "GetVPCLink"
    "fixture/GetVPCLink.yaml"

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

requestDeleteAPIKey :: DeleteAPIKey -> TestTree
requestDeleteAPIKey =
  req
    "DeleteAPIKey"
    "fixture/DeleteAPIKey.yaml"

requestUpdateAPIKey :: UpdateAPIKey -> TestTree
requestUpdateAPIKey =
  req
    "UpdateAPIKey"
    "fixture/UpdateAPIKey.yaml"

requestGetRestAPI :: GetRestAPI -> TestTree
requestGetRestAPI =
  req
    "GetRestAPI"
    "fixture/GetRestAPI.yaml"

requestGetStages :: GetStages -> TestTree
requestGetStages =
  req
    "GetStages"
    "fixture/GetStages.yaml"

requestPutRestAPI :: PutRestAPI -> TestTree
requestPutRestAPI =
  req
    "PutRestAPI"
    "fixture/PutRestAPI.yaml"

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

requestUpdateRestAPI :: UpdateRestAPI -> TestTree
requestUpdateRestAPI =
  req
    "UpdateRestAPI"
    "fixture/UpdateRestAPI.yaml"

requestDeleteRestAPI :: DeleteRestAPI -> TestTree
requestDeleteRestAPI =
  req
    "DeleteRestAPI"
    "fixture/DeleteRestAPI.yaml"

requestImportAPIKeys :: ImportAPIKeys -> TestTree
requestImportAPIKeys =
  req
    "ImportAPIKeys"
    "fixture/ImportAPIKeys.yaml"

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

requestCreateVPCLink :: CreateVPCLink -> TestTree
requestCreateVPCLink =
  req
    "CreateVPCLink"
    "fixture/CreateVPCLink.yaml"

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

requestDeleteVPCLink :: DeleteVPCLink -> TestTree
requestDeleteVPCLink =
  req
    "DeleteVPCLink"
    "fixture/DeleteVPCLink.yaml"

requestUpdateVPCLink :: UpdateVPCLink -> TestTree
requestUpdateVPCLink =
  req
    "UpdateVPCLink"
    "fixture/UpdateVPCLink.yaml"

requestFlushStageCache :: FlushStageCache -> TestTree
requestFlushStageCache =
  req
    "FlushStageCache"
    "fixture/FlushStageCache.yaml"

requestCreateRestAPI :: CreateRestAPI -> TestTree
requestCreateRestAPI =
  req
    "CreateRestAPI"
    "fixture/CreateRestAPI.yaml"

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

requestGetSDK :: GetSDK -> TestTree
requestGetSDK =
  req
    "GetSDK"
    "fixture/GetSDK.yaml"

requestGetAPIKeys :: GetAPIKeys -> TestTree
requestGetAPIKeys =
  req
    "GetAPIKeys"
    "fixture/GetAPIKeys.yaml"

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

requestCreateAPIKey :: CreateAPIKey -> TestTree
requestCreateAPIKey =
  req
    "CreateAPIKey"
    "fixture/CreateAPIKey.yaml"

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

requestGetSDKTypes :: GetSDKTypes -> TestTree
requestGetSDKTypes =
  req
    "GetSDKTypes"
    "fixture/GetSDKTypes.yaml"

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

requestGetAPIKey :: GetAPIKey -> TestTree
requestGetAPIKey =
  req
    "GetAPIKey"
    "fixture/GetAPIKey.yaml"

-- Responses

responseGetResource :: Resource -> TestTree
responseGetResource =
  res
    "GetResourceResponse"
    "fixture/GetResourceResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetResource)

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments =
  res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetDeployments)

responseGetDeployment :: Deployment -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetDeployment)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags =
  res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetTags)

responseDeleteGatewayResponse :: DeleteGatewayResponseResponse -> TestTree
responseDeleteGatewayResponse =
  res
    "DeleteGatewayResponseResponse"
    "fixture/DeleteGatewayResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteGatewayResponse)

responseUpdateGatewayResponse :: GatewayResponse -> TestTree
responseUpdateGatewayResponse =
  res
    "UpdateGatewayResponseResponse"
    "fixture/UpdateGatewayResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateGatewayResponse)

responseCreateUsagePlan :: UsagePlan -> TestTree
responseCreateUsagePlan =
  res
    "CreateUsagePlanResponse"
    "fixture/CreateUsagePlanResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateUsagePlan)

responseGetDomainNames :: GetDomainNamesResponse -> TestTree
responseGetDomainNames =
  res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetDomainNames)

responseGetClientCertificate :: ClientCertificate -> TestTree
responseGetClientCertificate =
  res
    "GetClientCertificateResponse"
    "fixture/GetClientCertificateResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetClientCertificate)

responsePutGatewayResponse :: GatewayResponse -> TestTree
responsePutGatewayResponse =
  res
    "PutGatewayResponseResponse"
    "fixture/PutGatewayResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy PutGatewayResponse)

responseGetSDKType :: SDKType -> TestTree
responseGetSDKType =
  res
    "GetSDKTypeResponse"
    "fixture/GetSDKTypeResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetSDKType)

responseGetMethodResponse :: MethodResponse -> TestTree
responseGetMethodResponse =
  res
    "GetMethodResponseResponse"
    "fixture/GetMethodResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetMethodResponse)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels =
  res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetModels)

responseGetBasePathMapping :: BasePathMapping -> TestTree
responseGetBasePathMapping =
  res
    "GetBasePathMappingResponse"
    "fixture/GetBasePathMappingResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetBasePathMapping)

responseGetRequestValidators :: GetRequestValidatorsResponse -> TestTree
responseGetRequestValidators =
  res
    "GetRequestValidatorsResponse"
    "fixture/GetRequestValidatorsResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetRequestValidators)

responsePutMethodResponse :: MethodResponse -> TestTree
responsePutMethodResponse =
  res
    "PutMethodResponseResponse"
    "fixture/PutMethodResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy PutMethodResponse)

responseImportRestAPI :: RestAPI -> TestTree
responseImportRestAPI =
  res
    "ImportRestAPIResponse"
    "fixture/ImportRestAPIResponse.proto"
    apiGatewayService
    (Proxy :: Proxy ImportRestAPI)

responseDeleteMethodResponse :: DeleteMethodResponseResponse -> TestTree
responseDeleteMethodResponse =
  res
    "DeleteMethodResponseResponse"
    "fixture/DeleteMethodResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteMethodResponse)

responseUpdateMethodResponse :: MethodResponse -> TestTree
responseUpdateMethodResponse =
  res
    "UpdateMethodResponseResponse"
    "fixture/UpdateMethodResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateMethodResponse)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage =
  res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteStage)

responseUpdateStage :: Stage -> TestTree
responseUpdateStage =
  res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateStage)

responseGetRestAPIs :: GetRestAPIsResponse -> TestTree
responseGetRestAPIs =
  res
    "GetRestAPIsResponse"
    "fixture/GetRestAPIsResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetRestAPIs)

responseGetDocumentationVersions :: GetDocumentationVersionsResponse -> TestTree
responseGetDocumentationVersions =
  res
    "GetDocumentationVersionsResponse"
    "fixture/GetDocumentationVersionsResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetDocumentationVersions)

responseCreateDeployment :: Deployment -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateDeployment)

responseGetVPCLinks :: GetVPCLinksResponse -> TestTree
responseGetVPCLinks =
  res
    "GetVPCLinksResponse"
    "fixture/GetVPCLinksResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetVPCLinks)

responseCreateBasePathMapping :: BasePathMapping -> TestTree
responseCreateBasePathMapping =
  res
    "CreateBasePathMappingResponse"
    "fixture/CreateBasePathMappingResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateBasePathMapping)

responseGetIntegration :: Integration -> TestTree
responseGetIntegration =
  res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetIntegration)

responseGetDocumentationParts :: GetDocumentationPartsResponse -> TestTree
responseGetDocumentationParts =
  res
    "GetDocumentationPartsResponse"
    "fixture/GetDocumentationPartsResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetDocumentationParts)

responseUpdateAccount :: Account -> TestTree
responseUpdateAccount =
  res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateAccount)

responseGetUsagePlan :: UsagePlan -> TestTree
responseGetUsagePlan =
  res
    "GetUsagePlanResponse"
    "fixture/GetUsagePlanResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetUsagePlan)

responseDeleteDeployment :: DeleteDeploymentResponse -> TestTree
responseDeleteDeployment =
  res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteDeployment)

responseUpdateDeployment :: Deployment -> TestTree
responseUpdateDeployment =
  res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateDeployment)

responseGetDocumentationPart :: DocumentationPart -> TestTree
responseGetDocumentationPart =
  res
    "GetDocumentationPartResponse"
    "fixture/GetDocumentationPartResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetDocumentationPart)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteResource)

responseUpdateResource :: Resource -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateResource)

responseCreateRequestValidator :: RequestValidator -> TestTree
responseCreateRequestValidator =
  res
    "CreateRequestValidatorResponse"
    "fixture/CreateRequestValidatorResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateRequestValidator)

responseImportDocumentationParts :: ImportDocumentationPartsResponse -> TestTree
responseImportDocumentationParts =
  res
    "ImportDocumentationPartsResponse"
    "fixture/ImportDocumentationPartsResponse.proto"
    apiGatewayService
    (Proxy :: Proxy ImportDocumentationParts)

responseGetUsage :: Usage -> TestTree
responseGetUsage =
  res
    "GetUsageResponse"
    "fixture/GetUsageResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetUsage)

responseGetVPCLink :: VPCLink -> TestTree
responseGetVPCLink =
  res
    "GetVPCLinkResponse"
    "fixture/GetVPCLinkResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetVPCLink)

responseCreateModel :: Model -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateModel)

responseGetIntegrationResponse :: IntegrationResponse -> TestTree
responseGetIntegrationResponse =
  res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetIntegrationResponse)

responseCreateDomainName :: DomainName -> TestTree
responseCreateDomainName =
  res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateDomainName)

responseFlushStageAuthorizersCache :: FlushStageAuthorizersCacheResponse -> TestTree
responseFlushStageAuthorizersCache =
  res
    "FlushStageAuthorizersCacheResponse"
    "fixture/FlushStageAuthorizersCacheResponse.proto"
    apiGatewayService
    (Proxy :: Proxy FlushStageAuthorizersCache)

responseGetGatewayResponses :: GetGatewayResponsesResponse -> TestTree
responseGetGatewayResponses =
  res
    "GetGatewayResponsesResponse"
    "fixture/GetGatewayResponsesResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetGatewayResponses)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteModel)

responseUpdateModel :: Model -> TestTree
responseUpdateModel =
  res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateModel)

responseGetDocumentationVersion :: DocumentationVersion -> TestTree
responseGetDocumentationVersion =
  res
    "GetDocumentationVersionResponse"
    "fixture/GetDocumentationVersionResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetDocumentationVersion)

responseDeleteAPIKey :: DeleteAPIKeyResponse -> TestTree
responseDeleteAPIKey =
  res
    "DeleteAPIKeyResponse"
    "fixture/DeleteAPIKeyResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteAPIKey)

responseUpdateAPIKey :: APIKey -> TestTree
responseUpdateAPIKey =
  res
    "UpdateAPIKeyResponse"
    "fixture/UpdateAPIKeyResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateAPIKey)

responseGetRestAPI :: RestAPI -> TestTree
responseGetRestAPI =
  res
    "GetRestAPIResponse"
    "fixture/GetRestAPIResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetRestAPI)

responseGetStages :: GetStagesResponse -> TestTree
responseGetStages =
  res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetStages)

responsePutRestAPI :: RestAPI -> TestTree
responsePutRestAPI =
  res
    "PutRestAPIResponse"
    "fixture/PutRestAPIResponse.proto"
    apiGatewayService
    (Proxy :: Proxy PutRestAPI)

responseGetMethod :: Method -> TestTree
responseGetMethod =
  res
    "GetMethodResponse"
    "fixture/GetMethodResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetMethod)

responseGetModel :: Model -> TestTree
responseGetModel =
  res
    "GetModelResponse"
    "fixture/GetModelResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetModel)

responseUpdateRestAPI :: RestAPI -> TestTree
responseUpdateRestAPI =
  res
    "UpdateRestAPIResponse"
    "fixture/UpdateRestAPIResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateRestAPI)

responseDeleteRestAPI :: DeleteRestAPIResponse -> TestTree
responseDeleteRestAPI =
  res
    "DeleteRestAPIResponse"
    "fixture/DeleteRestAPIResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteRestAPI)

responseImportAPIKeys :: ImportAPIKeysResponse -> TestTree
responseImportAPIKeys =
  res
    "ImportAPIKeysResponse"
    "fixture/ImportAPIKeysResponse.proto"
    apiGatewayService
    (Proxy :: Proxy ImportAPIKeys)

responseCreateDocumentationPart :: DocumentationPart -> TestTree
responseCreateDocumentationPart =
  res
    "CreateDocumentationPartResponse"
    "fixture/CreateDocumentationPartResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateDocumentationPart)

responseTestInvokeMethod :: TestInvokeMethodResponse -> TestTree
responseTestInvokeMethod =
  res
    "TestInvokeMethodResponse"
    "fixture/TestInvokeMethodResponse.proto"
    apiGatewayService
    (Proxy :: Proxy TestInvokeMethod)

responseGetRequestValidator :: RequestValidator -> TestTree
responseGetRequestValidator =
  res
    "GetRequestValidatorResponse"
    "fixture/GetRequestValidatorResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetRequestValidator)

responseGetDomainName :: DomainName -> TestTree
responseGetDomainName =
  res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetDomainName)

responseCreateVPCLink :: VPCLink -> TestTree
responseCreateVPCLink =
  res
    "CreateVPCLinkResponse"
    "fixture/CreateVPCLinkResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateVPCLink)

responseDeleteDocumentationPart :: DeleteDocumentationPartResponse -> TestTree
responseDeleteDocumentationPart =
  res
    "DeleteDocumentationPartResponse"
    "fixture/DeleteDocumentationPartResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteDocumentationPart)

responseUpdateDocumentationPart :: DocumentationPart -> TestTree
responseUpdateDocumentationPart =
  res
    "UpdateDocumentationPartResponse"
    "fixture/UpdateDocumentationPartResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateDocumentationPart)

responseGetAuthorizers :: GetAuthorizersResponse -> TestTree
responseGetAuthorizers =
  res
    "GetAuthorizersResponse"
    "fixture/GetAuthorizersResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetAuthorizers)

responseCreateDocumentationVersion :: DocumentationVersion -> TestTree
responseCreateDocumentationVersion =
  res
    "CreateDocumentationVersionResponse"
    "fixture/CreateDocumentationVersionResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateDocumentationVersion)

responsePutIntegrationResponse :: IntegrationResponse -> TestTree
responsePutIntegrationResponse =
  res
    "PutIntegrationResponseResponse"
    "fixture/PutIntegrationResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy PutIntegrationResponse)

responseGetUsagePlanKeys :: GetUsagePlanKeysResponse -> TestTree
responseGetUsagePlanKeys =
  res
    "GetUsagePlanKeysResponse"
    "fixture/GetUsagePlanKeysResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetUsagePlanKeys)

responseDeleteVPCLink :: DeleteVPCLinkResponse -> TestTree
responseDeleteVPCLink =
  res
    "DeleteVPCLinkResponse"
    "fixture/DeleteVPCLinkResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteVPCLink)

responseUpdateVPCLink :: VPCLink -> TestTree
responseUpdateVPCLink =
  res
    "UpdateVPCLinkResponse"
    "fixture/UpdateVPCLinkResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateVPCLink)

responseFlushStageCache :: FlushStageCacheResponse -> TestTree
responseFlushStageCache =
  res
    "FlushStageCacheResponse"
    "fixture/FlushStageCacheResponse.proto"
    apiGatewayService
    (Proxy :: Proxy FlushStageCache)

responseCreateRestAPI :: RestAPI -> TestTree
responseCreateRestAPI =
  res
    "CreateRestAPIResponse"
    "fixture/CreateRestAPIResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateRestAPI)

responseDeleteIntegrationResponse :: DeleteIntegrationResponseResponse -> TestTree
responseDeleteIntegrationResponse =
  res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteIntegrationResponse)

responseUpdateIntegrationResponse :: IntegrationResponse -> TestTree
responseUpdateIntegrationResponse =
  res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateIntegrationResponse)

responseUpdateUsage :: Usage -> TestTree
responseUpdateUsage =
  res
    "UpdateUsageResponse"
    "fixture/UpdateUsageResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateUsage)

responseDeleteIntegration :: DeleteIntegrationResponse' -> TestTree
responseDeleteIntegration =
  res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteIntegration)

responseUpdateIntegration :: Integration -> TestTree
responseUpdateIntegration =
  res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateIntegration)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer =
  res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    apiGatewayService
    (Proxy :: Proxy TestInvokeAuthorizer)

responseGenerateClientCertificate :: ClientCertificate -> TestTree
responseGenerateClientCertificate =
  res
    "GenerateClientCertificateResponse"
    "fixture/GenerateClientCertificateResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GenerateClientCertificate)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetResources)

responseGetUsagePlanKey :: UsagePlanKey -> TestTree
responseGetUsagePlanKey =
  res
    "GetUsagePlanKeyResponse"
    "fixture/GetUsagePlanKeyResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetUsagePlanKey)

responseGetAccount :: Account -> TestTree
responseGetAccount =
  res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetAccount)

responsePutIntegration :: Integration -> TestTree
responsePutIntegration =
  res
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.proto"
    apiGatewayService
    (Proxy :: Proxy PutIntegration)

responseGetAuthorizer :: Authorizer -> TestTree
responseGetAuthorizer =
  res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetAuthorizer)

responseDeleteUsagePlan :: DeleteUsagePlanResponse -> TestTree
responseDeleteUsagePlan =
  res
    "DeleteUsagePlanResponse"
    "fixture/DeleteUsagePlanResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteUsagePlan)

responseUpdateUsagePlan :: UsagePlan -> TestTree
responseUpdateUsagePlan =
  res
    "UpdateUsagePlanResponse"
    "fixture/UpdateUsagePlanResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateUsagePlan)

responseGetStage :: Stage -> TestTree
responseGetStage =
  res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetStage)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport =
  res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetExport)

responseGetSDK :: GetSDKResponse -> TestTree
responseGetSDK =
  res
    "GetSDKResponse"
    "fixture/GetSDKResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetSDK)

responseGetAPIKeys :: GetAPIKeysResponse -> TestTree
responseGetAPIKeys =
  res
    "GetAPIKeysResponse"
    "fixture/GetAPIKeysResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetAPIKeys)

responseDeleteBasePathMapping :: DeleteBasePathMappingResponse -> TestTree
responseDeleteBasePathMapping =
  res
    "DeleteBasePathMappingResponse"
    "fixture/DeleteBasePathMappingResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteBasePathMapping)

responseUpdateBasePathMapping :: BasePathMapping -> TestTree
responseUpdateBasePathMapping =
  res
    "UpdateBasePathMappingResponse"
    "fixture/UpdateBasePathMappingResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateBasePathMapping)

responseDeleteClientCertificate :: DeleteClientCertificateResponse -> TestTree
responseDeleteClientCertificate =
  res
    "DeleteClientCertificateResponse"
    "fixture/DeleteClientCertificateResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteClientCertificate)

responseUpdateClientCertificate :: ClientCertificate -> TestTree
responseUpdateClientCertificate =
  res
    "UpdateClientCertificateResponse"
    "fixture/UpdateClientCertificateResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateClientCertificate)

responseGetGatewayResponse :: GatewayResponse -> TestTree
responseGetGatewayResponse =
  res
    "GetGatewayResponseResponse"
    "fixture/GetGatewayResponseResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetGatewayResponse)

responseCreateUsagePlanKey :: UsagePlanKey -> TestTree
responseCreateUsagePlanKey =
  res
    "CreateUsagePlanKeyResponse"
    "fixture/CreateUsagePlanKeyResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateUsagePlanKey)

responseCreateAuthorizer :: Authorizer -> TestTree
responseCreateAuthorizer =
  res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateAuthorizer)

responseUpdateAuthorizer :: Authorizer -> TestTree
responseUpdateAuthorizer =
  res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateAuthorizer)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer =
  res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteAuthorizer)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    apiGatewayService
    (Proxy :: Proxy TagResource)

responseCreateStage :: Stage -> TestTree
responseCreateStage =
  res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateStage)

responseDeleteUsagePlanKey :: DeleteUsagePlanKeyResponse -> TestTree
responseDeleteUsagePlanKey =
  res
    "DeleteUsagePlanKeyResponse"
    "fixture/DeleteUsagePlanKeyResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteUsagePlanKey)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UntagResource)

responseCreateAPIKey :: APIKey -> TestTree
responseCreateAPIKey =
  res
    "CreateAPIKeyResponse"
    "fixture/CreateAPIKeyResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateAPIKey)

responseGetUsagePlans :: GetUsagePlansResponse -> TestTree
responseGetUsagePlans =
  res
    "GetUsagePlansResponse"
    "fixture/GetUsagePlansResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetUsagePlans)

responsePutMethod :: Method -> TestTree
responsePutMethod =
  res
    "PutMethodResponse"
    "fixture/PutMethodResponse.proto"
    apiGatewayService
    (Proxy :: Proxy PutMethod)

responseUpdateDomainName :: DomainName -> TestTree
responseUpdateDomainName =
  res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateDomainName)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName =
  res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteDomainName)

responseCreateResource :: Resource -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    apiGatewayService
    (Proxy :: Proxy CreateResource)

responseDeleteMethod :: DeleteMethodResponse' -> TestTree
responseDeleteMethod =
  res
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteMethod)

responseUpdateMethod :: Method -> TestTree
responseUpdateMethod =
  res
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateMethod)

responseUpdateRequestValidator :: RequestValidator -> TestTree
responseUpdateRequestValidator =
  res
    "UpdateRequestValidatorResponse"
    "fixture/UpdateRequestValidatorResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateRequestValidator)

responseDeleteRequestValidator :: DeleteRequestValidatorResponse -> TestTree
responseDeleteRequestValidator =
  res
    "DeleteRequestValidatorResponse"
    "fixture/DeleteRequestValidatorResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteRequestValidator)

responseGetSDKTypes :: GetSDKTypesResponse -> TestTree
responseGetSDKTypes =
  res
    "GetSDKTypesResponse"
    "fixture/GetSDKTypesResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetSDKTypes)

responseGetClientCertificates :: GetClientCertificatesResponse -> TestTree
responseGetClientCertificates =
  res
    "GetClientCertificatesResponse"
    "fixture/GetClientCertificatesResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetClientCertificates)

responseGetModelTemplate :: GetModelTemplateResponse -> TestTree
responseGetModelTemplate =
  res
    "GetModelTemplateResponse"
    "fixture/GetModelTemplateResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetModelTemplate)

responseUpdateDocumentationVersion :: DocumentationVersion -> TestTree
responseUpdateDocumentationVersion =
  res
    "UpdateDocumentationVersionResponse"
    "fixture/UpdateDocumentationVersionResponse.proto"
    apiGatewayService
    (Proxy :: Proxy UpdateDocumentationVersion)

responseDeleteDocumentationVersion :: DeleteDocumentationVersionResponse -> TestTree
responseDeleteDocumentationVersion =
  res
    "DeleteDocumentationVersionResponse"
    "fixture/DeleteDocumentationVersionResponse.proto"
    apiGatewayService
    (Proxy :: Proxy DeleteDocumentationVersion)

responseGetBasePathMappings :: GetBasePathMappingsResponse -> TestTree
responseGetBasePathMappings =
  res
    "GetBasePathMappingsResponse"
    "fixture/GetBasePathMappingsResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetBasePathMappings)

responseGetAPIKey :: APIKey -> TestTree
responseGetAPIKey =
  res
    "GetAPIKeyResponse"
    "fixture/GetAPIKeyResponse.proto"
    apiGatewayService
    (Proxy :: Proxy GetAPIKey)
