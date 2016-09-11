{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.APIGateway
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.APIGateway where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.APIGateway
import Test.AWS.APIGateway.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetResource $
--             getResource
--
--         , requestGetDeployments $
--             getDeployments
--
--         , requestGetDeployment $
--             getDeployment
--
--         , requestCreateUsagePlan $
--             createUsagePlan
--
--         , requestGetDomainNames $
--             getDomainNames
--
--         , requestGetClientCertificate $
--             getClientCertificate
--
--         , requestGetMethodResponse $
--             getMethodResponse
--
--         , requestGetModels $
--             getModels
--
--         , requestGetBasePathMapping $
--             getBasePathMapping
--
--         , requestPutMethodResponse $
--             putMethodResponse
--
--         , requestImportRestAPI $
--             importRestAPI
--
--         , requestDeleteMethodResponse $
--             deleteMethodResponse
--
--         , requestUpdateMethodResponse $
--             updateMethodResponse
--
--         , requestDeleteStage $
--             deleteStage
--
--         , requestUpdateStage $
--             updateStage
--
--         , requestGetRestAPIs $
--             getRestAPIs
--
--         , requestCreateDeployment $
--             createDeployment
--
--         , requestCreateBasePathMapping $
--             createBasePathMapping
--
--         , requestGetIntegration $
--             getIntegration
--
--         , requestUpdateAccount $
--             updateAccount
--
--         , requestGetUsagePlan $
--             getUsagePlan
--
--         , requestDeleteDeployment $
--             deleteDeployment
--
--         , requestUpdateDeployment $
--             updateDeployment
--
--         , requestDeleteResource $
--             deleteResource
--
--         , requestUpdateResource $
--             updateResource
--
--         , requestGetUsage $
--             getUsage
--
--         , requestCreateModel $
--             createModel
--
--         , requestGetIntegrationResponse $
--             getIntegrationResponse
--
--         , requestCreateDomainName $
--             createDomainName
--
--         , requestFlushStageAuthorizersCache $
--             flushStageAuthorizersCache
--
--         , requestDeleteModel $
--             deleteModel
--
--         , requestUpdateModel $
--             updateModel
--
--         , requestDeleteAPIKey $
--             deleteAPIKey
--
--         , requestUpdateAPIKey $
--             updateAPIKey
--
--         , requestGetRestAPI $
--             getRestAPI
--
--         , requestGetStages $
--             getStages
--
--         , requestPutRestAPI $
--             putRestAPI
--
--         , requestGetMethod $
--             getMethod
--
--         , requestGetModel $
--             getModel
--
--         , requestUpdateRestAPI $
--             updateRestAPI
--
--         , requestDeleteRestAPI $
--             deleteRestAPI
--
--         , requestImportAPIKeys $
--             importAPIKeys
--
--         , requestTestInvokeMethod $
--             testInvokeMethod
--
--         , requestGetDomainName $
--             getDomainName
--
--         , requestGetAuthorizers $
--             getAuthorizers
--
--         , requestPutIntegrationResponse $
--             putIntegrationResponse
--
--         , requestGetUsagePlanKeys $
--             getUsagePlanKeys
--
--         , requestFlushStageCache $
--             flushStageCache
--
--         , requestCreateRestAPI $
--             createRestAPI
--
--         , requestDeleteIntegrationResponse $
--             deleteIntegrationResponse
--
--         , requestUpdateIntegrationResponse $
--             updateIntegrationResponse
--
--         , requestUpdateUsage $
--             updateUsage
--
--         , requestDeleteIntegration $
--             deleteIntegration
--
--         , requestUpdateIntegration $
--             updateIntegration
--
--         , requestTestInvokeAuthorizer $
--             testInvokeAuthorizer
--
--         , requestGenerateClientCertificate $
--             generateClientCertificate
--
--         , requestGetResources $
--             getResources
--
--         , requestGetUsagePlanKey $
--             getUsagePlanKey
--
--         , requestGetAccount $
--             getAccount
--
--         , requestPutIntegration $
--             putIntegration
--
--         , requestGetAuthorizer $
--             getAuthorizer
--
--         , requestDeleteUsagePlan $
--             deleteUsagePlan
--
--         , requestUpdateUsagePlan $
--             updateUsagePlan
--
--         , requestGetStage $
--             getStage
--
--         , requestGetExport $
--             getExport
--
--         , requestGetSDK $
--             getSDK
--
--         , requestGetAPIKeys $
--             getAPIKeys
--
--         , requestDeleteBasePathMapping $
--             deleteBasePathMapping
--
--         , requestUpdateBasePathMapping $
--             updateBasePathMapping
--
--         , requestDeleteClientCertificate $
--             deleteClientCertificate
--
--         , requestUpdateClientCertificate $
--             updateClientCertificate
--
--         , requestCreateUsagePlanKey $
--             createUsagePlanKey
--
--         , requestCreateAuthorizer $
--             createAuthorizer
--
--         , requestUpdateAuthorizer $
--             updateAuthorizer
--
--         , requestDeleteAuthorizer $
--             deleteAuthorizer
--
--         , requestCreateStage $
--             createStage
--
--         , requestDeleteUsagePlanKey $
--             deleteUsagePlanKey
--
--         , requestCreateAPIKey $
--             createAPIKey
--
--         , requestGetUsagePlans $
--             getUsagePlans
--
--         , requestPutMethod $
--             putMethod
--
--         , requestUpdateDomainName $
--             updateDomainName
--
--         , requestDeleteDomainName $
--             deleteDomainName
--
--         , requestCreateResource $
--             createResource
--
--         , requestDeleteMethod $
--             deleteMethod
--
--         , requestUpdateMethod $
--             updateMethod
--
--         , requestGetClientCertificates $
--             getClientCertificates
--
--         , requestGetModelTemplate $
--             getModelTemplate
--
--         , requestGetBasePathMappings $
--             getBasePathMappings
--
--         , requestGetAPIKey $
--             getAPIKey
--
--           ]

--     , testGroup "response"
--         [ responseGetResource $
--             resource
--
--         , responseGetDeployments $
--             getDeploymentsResponse
--
--         , responseGetDeployment $
--             deployment
--
--         , responseCreateUsagePlan $
--             usagePlan
--
--         , responseGetDomainNames $
--             getDomainNamesResponse
--
--         , responseGetClientCertificate $
--             clientCertificate
--
--         , responseGetMethodResponse $
--             methodResponse
--
--         , responseGetModels $
--             getModelsResponse
--
--         , responseGetBasePathMapping $
--             basePathMapping
--
--         , responsePutMethodResponse $
--             methodResponse
--
--         , responseImportRestAPI $
--             restAPI
--
--         , responseDeleteMethodResponse $
--             deleteMethodResponseResponse
--
--         , responseUpdateMethodResponse $
--             methodResponse
--
--         , responseDeleteStage $
--             deleteStageResponse
--
--         , responseUpdateStage $
--             stage
--
--         , responseGetRestAPIs $
--             getRestAPIsResponse
--
--         , responseCreateDeployment $
--             deployment
--
--         , responseCreateBasePathMapping $
--             basePathMapping
--
--         , responseGetIntegration $
--             integration
--
--         , responseUpdateAccount $
--             account
--
--         , responseGetUsagePlan $
--             usagePlan
--
--         , responseDeleteDeployment $
--             deleteDeploymentResponse
--
--         , responseUpdateDeployment $
--             deployment
--
--         , responseDeleteResource $
--             deleteResourceResponse
--
--         , responseUpdateResource $
--             resource
--
--         , responseGetUsage $
--             usage
--
--         , responseCreateModel $
--             model
--
--         , responseGetIntegrationResponse $
--             integrationResponse
--
--         , responseCreateDomainName $
--             domainName
--
--         , responseFlushStageAuthorizersCache $
--             flushStageAuthorizersCacheResponse
--
--         , responseDeleteModel $
--             deleteModelResponse
--
--         , responseUpdateModel $
--             model
--
--         , responseDeleteAPIKey $
--             deleteAPIKeyResponse
--
--         , responseUpdateAPIKey $
--             apiKey
--
--         , responseGetRestAPI $
--             restAPI
--
--         , responseGetStages $
--             getStagesResponse
--
--         , responsePutRestAPI $
--             restAPI
--
--         , responseGetMethod $
--             method
--
--         , responseGetModel $
--             model
--
--         , responseUpdateRestAPI $
--             restAPI
--
--         , responseDeleteRestAPI $
--             deleteRestAPIResponse
--
--         , responseImportAPIKeys $
--             importAPIKeysResponse
--
--         , responseTestInvokeMethod $
--             testInvokeMethodResponse
--
--         , responseGetDomainName $
--             domainName
--
--         , responseGetAuthorizers $
--             getAuthorizersResponse
--
--         , responsePutIntegrationResponse $
--             integrationResponse
--
--         , responseGetUsagePlanKeys $
--             getUsagePlanKeysResponse
--
--         , responseFlushStageCache $
--             flushStageCacheResponse
--
--         , responseCreateRestAPI $
--             restAPI
--
--         , responseDeleteIntegrationResponse $
--             deleteIntegrationResponseResponse
--
--         , responseUpdateIntegrationResponse $
--             integrationResponse
--
--         , responseUpdateUsage $
--             usage
--
--         , responseDeleteIntegration $
--             deleteIntegrationResponse'
--
--         , responseUpdateIntegration $
--             integration
--
--         , responseTestInvokeAuthorizer $
--             testInvokeAuthorizerResponse
--
--         , responseGenerateClientCertificate $
--             clientCertificate
--
--         , responseGetResources $
--             getResourcesResponse
--
--         , responseGetUsagePlanKey $
--             usagePlanKey
--
--         , responseGetAccount $
--             account
--
--         , responsePutIntegration $
--             integration
--
--         , responseGetAuthorizer $
--             authorizer
--
--         , responseDeleteUsagePlan $
--             deleteUsagePlanResponse
--
--         , responseUpdateUsagePlan $
--             usagePlan
--
--         , responseGetStage $
--             stage
--
--         , responseGetExport $
--             getExportResponse
--
--         , responseGetSDK $
--             getSDKResponse
--
--         , responseGetAPIKeys $
--             getAPIKeysResponse
--
--         , responseDeleteBasePathMapping $
--             deleteBasePathMappingResponse
--
--         , responseUpdateBasePathMapping $
--             basePathMapping
--
--         , responseDeleteClientCertificate $
--             deleteClientCertificateResponse
--
--         , responseUpdateClientCertificate $
--             clientCertificate
--
--         , responseCreateUsagePlanKey $
--             usagePlanKey
--
--         , responseCreateAuthorizer $
--             authorizer
--
--         , responseUpdateAuthorizer $
--             authorizer
--
--         , responseDeleteAuthorizer $
--             deleteAuthorizerResponse
--
--         , responseCreateStage $
--             stage
--
--         , responseDeleteUsagePlanKey $
--             deleteUsagePlanKeyResponse
--
--         , responseCreateAPIKey $
--             apiKey
--
--         , responseGetUsagePlans $
--             getUsagePlansResponse
--
--         , responsePutMethod $
--             method
--
--         , responseUpdateDomainName $
--             domainName
--
--         , responseDeleteDomainName $
--             deleteDomainNameResponse
--
--         , responseCreateResource $
--             resource
--
--         , responseDeleteMethod $
--             deleteMethodResponse'
--
--         , responseUpdateMethod $
--             method
--
--         , responseGetClientCertificates $
--             getClientCertificatesResponse
--
--         , responseGetModelTemplate $
--             getModelTemplateResponse
--
--         , responseGetBasePathMappings $
--             getBasePathMappingsResponse
--
--         , responseGetAPIKey $
--             apiKey
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

requestPutMethodResponse :: PutMethodResponse -> TestTree
requestPutMethodResponse = req
    "PutMethodResponse"
    "fixture/PutMethodResponse.yaml"

requestImportRestAPI :: ImportRestAPI -> TestTree
requestImportRestAPI = req
    "ImportRestAPI"
    "fixture/ImportRestAPI.yaml"

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

requestGetRestAPIs :: GetRestAPIs -> TestTree
requestGetRestAPIs = req
    "GetRestAPIs"
    "fixture/GetRestAPIs.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment = req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestCreateBasePathMapping :: CreateBasePathMapping -> TestTree
requestCreateBasePathMapping = req
    "CreateBasePathMapping"
    "fixture/CreateBasePathMapping.yaml"

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration = req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

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

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource = req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource = req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestGetUsage :: GetUsage -> TestTree
requestGetUsage = req
    "GetUsage"
    "fixture/GetUsage.yaml"

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

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel = req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestUpdateModel :: UpdateModel -> TestTree
requestUpdateModel = req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

requestDeleteAPIKey :: DeleteAPIKey -> TestTree
requestDeleteAPIKey = req
    "DeleteAPIKey"
    "fixture/DeleteAPIKey.yaml"

requestUpdateAPIKey :: UpdateAPIKey -> TestTree
requestUpdateAPIKey = req
    "UpdateAPIKey"
    "fixture/UpdateAPIKey.yaml"

requestGetRestAPI :: GetRestAPI -> TestTree
requestGetRestAPI = req
    "GetRestAPI"
    "fixture/GetRestAPI.yaml"

requestGetStages :: GetStages -> TestTree
requestGetStages = req
    "GetStages"
    "fixture/GetStages.yaml"

requestPutRestAPI :: PutRestAPI -> TestTree
requestPutRestAPI = req
    "PutRestAPI"
    "fixture/PutRestAPI.yaml"

requestGetMethod :: GetMethod -> TestTree
requestGetMethod = req
    "GetMethod"
    "fixture/GetMethod.yaml"

requestGetModel :: GetModel -> TestTree
requestGetModel = req
    "GetModel"
    "fixture/GetModel.yaml"

requestUpdateRestAPI :: UpdateRestAPI -> TestTree
requestUpdateRestAPI = req
    "UpdateRestAPI"
    "fixture/UpdateRestAPI.yaml"

requestDeleteRestAPI :: DeleteRestAPI -> TestTree
requestDeleteRestAPI = req
    "DeleteRestAPI"
    "fixture/DeleteRestAPI.yaml"

requestImportAPIKeys :: ImportAPIKeys -> TestTree
requestImportAPIKeys = req
    "ImportAPIKeys"
    "fixture/ImportAPIKeys.yaml"

requestTestInvokeMethod :: TestInvokeMethod -> TestTree
requestTestInvokeMethod = req
    "TestInvokeMethod"
    "fixture/TestInvokeMethod.yaml"

requestGetDomainName :: GetDomainName -> TestTree
requestGetDomainName = req
    "GetDomainName"
    "fixture/GetDomainName.yaml"

requestGetAuthorizers :: GetAuthorizers -> TestTree
requestGetAuthorizers = req
    "GetAuthorizers"
    "fixture/GetAuthorizers.yaml"

requestPutIntegrationResponse :: PutIntegrationResponse -> TestTree
requestPutIntegrationResponse = req
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.yaml"

requestGetUsagePlanKeys :: GetUsagePlanKeys -> TestTree
requestGetUsagePlanKeys = req
    "GetUsagePlanKeys"
    "fixture/GetUsagePlanKeys.yaml"

requestFlushStageCache :: FlushStageCache -> TestTree
requestFlushStageCache = req
    "FlushStageCache"
    "fixture/FlushStageCache.yaml"

requestCreateRestAPI :: CreateRestAPI -> TestTree
requestCreateRestAPI = req
    "CreateRestAPI"
    "fixture/CreateRestAPI.yaml"

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

requestGetSDK :: GetSDK -> TestTree
requestGetSDK = req
    "GetSDK"
    "fixture/GetSDK.yaml"

requestGetAPIKeys :: GetAPIKeys -> TestTree
requestGetAPIKeys = req
    "GetAPIKeys"
    "fixture/GetAPIKeys.yaml"

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

requestCreateStage :: CreateStage -> TestTree
requestCreateStage = req
    "CreateStage"
    "fixture/CreateStage.yaml"

requestDeleteUsagePlanKey :: DeleteUsagePlanKey -> TestTree
requestDeleteUsagePlanKey = req
    "DeleteUsagePlanKey"
    "fixture/DeleteUsagePlanKey.yaml"

requestCreateAPIKey :: CreateAPIKey -> TestTree
requestCreateAPIKey = req
    "CreateAPIKey"
    "fixture/CreateAPIKey.yaml"

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

requestGetClientCertificates :: GetClientCertificates -> TestTree
requestGetClientCertificates = req
    "GetClientCertificates"
    "fixture/GetClientCertificates.yaml"

requestGetModelTemplate :: GetModelTemplate -> TestTree
requestGetModelTemplate = req
    "GetModelTemplate"
    "fixture/GetModelTemplate.yaml"

requestGetBasePathMappings :: GetBasePathMappings -> TestTree
requestGetBasePathMappings = req
    "GetBasePathMappings"
    "fixture/GetBasePathMappings.yaml"

requestGetAPIKey :: GetAPIKey -> TestTree
requestGetAPIKey = req
    "GetAPIKey"
    "fixture/GetAPIKey.yaml"

-- Responses

responseGetResource :: Resource -> TestTree
responseGetResource = res
    "GetResourceResponse"
    "fixture/GetResourceResponse.proto"
    apiGateway
    (Proxy :: Proxy GetResource)

responseGetDeployments :: GetDeploymentsResponse -> TestTree
responseGetDeployments = res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    apiGateway
    (Proxy :: Proxy GetDeployments)

responseGetDeployment :: Deployment -> TestTree
responseGetDeployment = res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    apiGateway
    (Proxy :: Proxy GetDeployment)

responseCreateUsagePlan :: UsagePlan -> TestTree
responseCreateUsagePlan = res
    "CreateUsagePlanResponse"
    "fixture/CreateUsagePlanResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateUsagePlan)

responseGetDomainNames :: GetDomainNamesResponse -> TestTree
responseGetDomainNames = res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    apiGateway
    (Proxy :: Proxy GetDomainNames)

responseGetClientCertificate :: ClientCertificate -> TestTree
responseGetClientCertificate = res
    "GetClientCertificateResponse"
    "fixture/GetClientCertificateResponse.proto"
    apiGateway
    (Proxy :: Proxy GetClientCertificate)

responseGetMethodResponse :: MethodResponse -> TestTree
responseGetMethodResponse = res
    "GetMethodResponseResponse"
    "fixture/GetMethodResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy GetMethodResponse)

responseGetModels :: GetModelsResponse -> TestTree
responseGetModels = res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    apiGateway
    (Proxy :: Proxy GetModels)

responseGetBasePathMapping :: BasePathMapping -> TestTree
responseGetBasePathMapping = res
    "GetBasePathMappingResponse"
    "fixture/GetBasePathMappingResponse.proto"
    apiGateway
    (Proxy :: Proxy GetBasePathMapping)

responsePutMethodResponse :: MethodResponse -> TestTree
responsePutMethodResponse = res
    "PutMethodResponseResponse"
    "fixture/PutMethodResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy PutMethodResponse)

responseImportRestAPI :: RestAPI -> TestTree
responseImportRestAPI = res
    "ImportRestAPIResponse"
    "fixture/ImportRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy ImportRestAPI)

responseDeleteMethodResponse :: DeleteMethodResponseResponse -> TestTree
responseDeleteMethodResponse = res
    "DeleteMethodResponseResponse"
    "fixture/DeleteMethodResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteMethodResponse)

responseUpdateMethodResponse :: MethodResponse -> TestTree
responseUpdateMethodResponse = res
    "UpdateMethodResponseResponse"
    "fixture/UpdateMethodResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateMethodResponse)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage = res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteStage)

responseUpdateStage :: Stage -> TestTree
responseUpdateStage = res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateStage)

responseGetRestAPIs :: GetRestAPIsResponse -> TestTree
responseGetRestAPIs = res
    "GetRestAPIsResponse"
    "fixture/GetRestAPIsResponse.proto"
    apiGateway
    (Proxy :: Proxy GetRestAPIs)

responseCreateDeployment :: Deployment -> TestTree
responseCreateDeployment = res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateDeployment)

responseCreateBasePathMapping :: BasePathMapping -> TestTree
responseCreateBasePathMapping = res
    "CreateBasePathMappingResponse"
    "fixture/CreateBasePathMappingResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateBasePathMapping)

responseGetIntegration :: Integration -> TestTree
responseGetIntegration = res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    apiGateway
    (Proxy :: Proxy GetIntegration)

responseUpdateAccount :: Account -> TestTree
responseUpdateAccount = res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateAccount)

responseGetUsagePlan :: UsagePlan -> TestTree
responseGetUsagePlan = res
    "GetUsagePlanResponse"
    "fixture/GetUsagePlanResponse.proto"
    apiGateway
    (Proxy :: Proxy GetUsagePlan)

responseDeleteDeployment :: DeleteDeploymentResponse -> TestTree
responseDeleteDeployment = res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteDeployment)

responseUpdateDeployment :: Deployment -> TestTree
responseUpdateDeployment = res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateDeployment)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource = res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteResource)

responseUpdateResource :: Resource -> TestTree
responseUpdateResource = res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateResource)

responseGetUsage :: Usage -> TestTree
responseGetUsage = res
    "GetUsageResponse"
    "fixture/GetUsageResponse.proto"
    apiGateway
    (Proxy :: Proxy GetUsage)

responseCreateModel :: Model -> TestTree
responseCreateModel = res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateModel)

responseGetIntegrationResponse :: IntegrationResponse -> TestTree
responseGetIntegrationResponse = res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy GetIntegrationResponse)

responseCreateDomainName :: DomainName -> TestTree
responseCreateDomainName = res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateDomainName)

responseFlushStageAuthorizersCache :: FlushStageAuthorizersCacheResponse -> TestTree
responseFlushStageAuthorizersCache = res
    "FlushStageAuthorizersCacheResponse"
    "fixture/FlushStageAuthorizersCacheResponse.proto"
    apiGateway
    (Proxy :: Proxy FlushStageAuthorizersCache)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel = res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteModel)

responseUpdateModel :: Model -> TestTree
responseUpdateModel = res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateModel)

responseDeleteAPIKey :: DeleteAPIKeyResponse -> TestTree
responseDeleteAPIKey = res
    "DeleteAPIKeyResponse"
    "fixture/DeleteAPIKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteAPIKey)

responseUpdateAPIKey :: APIKey -> TestTree
responseUpdateAPIKey = res
    "UpdateAPIKeyResponse"
    "fixture/UpdateAPIKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateAPIKey)

responseGetRestAPI :: RestAPI -> TestTree
responseGetRestAPI = res
    "GetRestAPIResponse"
    "fixture/GetRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy GetRestAPI)

responseGetStages :: GetStagesResponse -> TestTree
responseGetStages = res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    apiGateway
    (Proxy :: Proxy GetStages)

responsePutRestAPI :: RestAPI -> TestTree
responsePutRestAPI = res
    "PutRestAPIResponse"
    "fixture/PutRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy PutRestAPI)

responseGetMethod :: Method -> TestTree
responseGetMethod = res
    "GetMethodResponse"
    "fixture/GetMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy GetMethod)

responseGetModel :: Model -> TestTree
responseGetModel = res
    "GetModelResponse"
    "fixture/GetModelResponse.proto"
    apiGateway
    (Proxy :: Proxy GetModel)

responseUpdateRestAPI :: RestAPI -> TestTree
responseUpdateRestAPI = res
    "UpdateRestAPIResponse"
    "fixture/UpdateRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateRestAPI)

responseDeleteRestAPI :: DeleteRestAPIResponse -> TestTree
responseDeleteRestAPI = res
    "DeleteRestAPIResponse"
    "fixture/DeleteRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteRestAPI)

responseImportAPIKeys :: ImportAPIKeysResponse -> TestTree
responseImportAPIKeys = res
    "ImportAPIKeysResponse"
    "fixture/ImportAPIKeysResponse.proto"
    apiGateway
    (Proxy :: Proxy ImportAPIKeys)

responseTestInvokeMethod :: TestInvokeMethodResponse -> TestTree
responseTestInvokeMethod = res
    "TestInvokeMethodResponse"
    "fixture/TestInvokeMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy TestInvokeMethod)

responseGetDomainName :: DomainName -> TestTree
responseGetDomainName = res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    apiGateway
    (Proxy :: Proxy GetDomainName)

responseGetAuthorizers :: GetAuthorizersResponse -> TestTree
responseGetAuthorizers = res
    "GetAuthorizersResponse"
    "fixture/GetAuthorizersResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAuthorizers)

responsePutIntegrationResponse :: IntegrationResponse -> TestTree
responsePutIntegrationResponse = res
    "PutIntegrationResponseResponse"
    "fixture/PutIntegrationResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy PutIntegrationResponse)

responseGetUsagePlanKeys :: GetUsagePlanKeysResponse -> TestTree
responseGetUsagePlanKeys = res
    "GetUsagePlanKeysResponse"
    "fixture/GetUsagePlanKeysResponse.proto"
    apiGateway
    (Proxy :: Proxy GetUsagePlanKeys)

responseFlushStageCache :: FlushStageCacheResponse -> TestTree
responseFlushStageCache = res
    "FlushStageCacheResponse"
    "fixture/FlushStageCacheResponse.proto"
    apiGateway
    (Proxy :: Proxy FlushStageCache)

responseCreateRestAPI :: RestAPI -> TestTree
responseCreateRestAPI = res
    "CreateRestAPIResponse"
    "fixture/CreateRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateRestAPI)

responseDeleteIntegrationResponse :: DeleteIntegrationResponseResponse -> TestTree
responseDeleteIntegrationResponse = res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteIntegrationResponse)

responseUpdateIntegrationResponse :: IntegrationResponse -> TestTree
responseUpdateIntegrationResponse = res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateIntegrationResponse)

responseUpdateUsage :: Usage -> TestTree
responseUpdateUsage = res
    "UpdateUsageResponse"
    "fixture/UpdateUsageResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateUsage)

responseDeleteIntegration :: DeleteIntegrationResponse' -> TestTree
responseDeleteIntegration = res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteIntegration)

responseUpdateIntegration :: Integration -> TestTree
responseUpdateIntegration = res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateIntegration)

responseTestInvokeAuthorizer :: TestInvokeAuthorizerResponse -> TestTree
responseTestInvokeAuthorizer = res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy TestInvokeAuthorizer)

responseGenerateClientCertificate :: ClientCertificate -> TestTree
responseGenerateClientCertificate = res
    "GenerateClientCertificateResponse"
    "fixture/GenerateClientCertificateResponse.proto"
    apiGateway
    (Proxy :: Proxy GenerateClientCertificate)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources = res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    apiGateway
    (Proxy :: Proxy GetResources)

responseGetUsagePlanKey :: UsagePlanKey -> TestTree
responseGetUsagePlanKey = res
    "GetUsagePlanKeyResponse"
    "fixture/GetUsagePlanKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy GetUsagePlanKey)

responseGetAccount :: Account -> TestTree
responseGetAccount = res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAccount)

responsePutIntegration :: Integration -> TestTree
responsePutIntegration = res
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.proto"
    apiGateway
    (Proxy :: Proxy PutIntegration)

responseGetAuthorizer :: Authorizer -> TestTree
responseGetAuthorizer = res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAuthorizer)

responseDeleteUsagePlan :: DeleteUsagePlanResponse -> TestTree
responseDeleteUsagePlan = res
    "DeleteUsagePlanResponse"
    "fixture/DeleteUsagePlanResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteUsagePlan)

responseUpdateUsagePlan :: UsagePlan -> TestTree
responseUpdateUsagePlan = res
    "UpdateUsagePlanResponse"
    "fixture/UpdateUsagePlanResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateUsagePlan)

responseGetStage :: Stage -> TestTree
responseGetStage = res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    apiGateway
    (Proxy :: Proxy GetStage)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport = res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    apiGateway
    (Proxy :: Proxy GetExport)

responseGetSDK :: GetSDKResponse -> TestTree
responseGetSDK = res
    "GetSDKResponse"
    "fixture/GetSDKResponse.proto"
    apiGateway
    (Proxy :: Proxy GetSDK)

responseGetAPIKeys :: GetAPIKeysResponse -> TestTree
responseGetAPIKeys = res
    "GetAPIKeysResponse"
    "fixture/GetAPIKeysResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAPIKeys)

responseDeleteBasePathMapping :: DeleteBasePathMappingResponse -> TestTree
responseDeleteBasePathMapping = res
    "DeleteBasePathMappingResponse"
    "fixture/DeleteBasePathMappingResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteBasePathMapping)

responseUpdateBasePathMapping :: BasePathMapping -> TestTree
responseUpdateBasePathMapping = res
    "UpdateBasePathMappingResponse"
    "fixture/UpdateBasePathMappingResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateBasePathMapping)

responseDeleteClientCertificate :: DeleteClientCertificateResponse -> TestTree
responseDeleteClientCertificate = res
    "DeleteClientCertificateResponse"
    "fixture/DeleteClientCertificateResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteClientCertificate)

responseUpdateClientCertificate :: ClientCertificate -> TestTree
responseUpdateClientCertificate = res
    "UpdateClientCertificateResponse"
    "fixture/UpdateClientCertificateResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateClientCertificate)

responseCreateUsagePlanKey :: UsagePlanKey -> TestTree
responseCreateUsagePlanKey = res
    "CreateUsagePlanKeyResponse"
    "fixture/CreateUsagePlanKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateUsagePlanKey)

responseCreateAuthorizer :: Authorizer -> TestTree
responseCreateAuthorizer = res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateAuthorizer)

responseUpdateAuthorizer :: Authorizer -> TestTree
responseUpdateAuthorizer = res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateAuthorizer)

responseDeleteAuthorizer :: DeleteAuthorizerResponse -> TestTree
responseDeleteAuthorizer = res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteAuthorizer)

responseCreateStage :: Stage -> TestTree
responseCreateStage = res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateStage)

responseDeleteUsagePlanKey :: DeleteUsagePlanKeyResponse -> TestTree
responseDeleteUsagePlanKey = res
    "DeleteUsagePlanKeyResponse"
    "fixture/DeleteUsagePlanKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteUsagePlanKey)

responseCreateAPIKey :: APIKey -> TestTree
responseCreateAPIKey = res
    "CreateAPIKeyResponse"
    "fixture/CreateAPIKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateAPIKey)

responseGetUsagePlans :: GetUsagePlansResponse -> TestTree
responseGetUsagePlans = res
    "GetUsagePlansResponse"
    "fixture/GetUsagePlansResponse.proto"
    apiGateway
    (Proxy :: Proxy GetUsagePlans)

responsePutMethod :: Method -> TestTree
responsePutMethod = res
    "PutMethodResponse"
    "fixture/PutMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy PutMethod)

responseUpdateDomainName :: DomainName -> TestTree
responseUpdateDomainName = res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateDomainName)

responseDeleteDomainName :: DeleteDomainNameResponse -> TestTree
responseDeleteDomainName = res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteDomainName)

responseCreateResource :: Resource -> TestTree
responseCreateResource = res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateResource)

responseDeleteMethod :: DeleteMethodResponse' -> TestTree
responseDeleteMethod = res
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteMethod)

responseUpdateMethod :: Method -> TestTree
responseUpdateMethod = res
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateMethod)

responseGetClientCertificates :: GetClientCertificatesResponse -> TestTree
responseGetClientCertificates = res
    "GetClientCertificatesResponse"
    "fixture/GetClientCertificatesResponse.proto"
    apiGateway
    (Proxy :: Proxy GetClientCertificates)

responseGetModelTemplate :: GetModelTemplateResponse -> TestTree
responseGetModelTemplate = res
    "GetModelTemplateResponse"
    "fixture/GetModelTemplateResponse.proto"
    apiGateway
    (Proxy :: Proxy GetModelTemplate)

responseGetBasePathMappings :: GetBasePathMappingsResponse -> TestTree
responseGetBasePathMappings = res
    "GetBasePathMappingsResponse"
    "fixture/GetBasePathMappingsResponse.proto"
    apiGateway
    (Proxy :: Proxy GetBasePathMappings)

responseGetAPIKey :: APIKey -> TestTree
responseGetAPIKey = res
    "GetAPIKeyResponse"
    "fixture/GetAPIKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAPIKey)
